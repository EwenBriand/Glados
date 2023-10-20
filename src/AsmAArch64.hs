{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AsmAArch64
  ( CodeState (..),
    Register,
    Label,
    RInstructionGen,
    RInstruction (..),
    emit,
    mov,
    ascii,
    label,
    exportSymbol,
    assemble,
    convertOneInstruction,
    encodeMovqRegImm,
    movqRegImmByteArray,
    byteStringToInteger,
    movOpcodeCombineRegReg,
    removeNullPrefix,
    binLengthFromRInstruction,
  )
where

import Control.Exception.ChainedException
import Control.Monad.Catch
import Control.Monad.State as MS
import Data.Bits
import Data.ByteString.Builder
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC
import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Data.Int
import Data.Kind
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Word
import Debug.Trace
import GHC.IO.Encoding (BufferCodec (encode))
import VM
import ValidState
import Prelude as P

data RegisterWidth = X | W

data SingRegisterWidth :: RegisterWidth -> Type where
  SX :: SingRegisterWidth 'X
  SW :: SingRegisterWidth 'W

class SingRegisterWidthI (c :: RegisterWidth) where
  singRegisterWidth :: SingRegisterWidth c

instance SingRegisterWidthI 'X where
  singRegisterWidth = SX

instance SingRegisterWidthI 'W where
  singRegisterWidth = SW

newtype RRegister (c :: RegisterWidth) = R Word32

newtype CodeOffset = CodeOffset {getCodeOffset :: Int64} deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits)

newtype RInstruction = RInstruction {getInstruction :: Integer} deriving (Eq, Show, Ord, Num) -- , Bits, FiniteBits, num, Real

data Label
  = CodeRef !CodeOffset
  | PoolRef !CodeOffset

-- Args:
-- Offset of the Rinstruction
-- Offset of the pool
type RInstructionGen = CodeOffset -> CodeOffset -> Either String RInstruction

data CodeState = CodeState
  { offsetInPool :: CodeOffset,
    poolReversed :: [Builder],
    codeReversed :: [RInstructionGen],
    symbolsRefersed :: [(String, Label)],
    codeBinLength :: Int
  }

binLengthFromInteger :: Integer -> Int
binLengthFromInteger i = P.length $ integerToWord8Array i

binLengthFromRInstruction :: RInstructionGen -> Int
binLengthFromRInstruction g = binLengthFromInteger $ getInstruction $ either error id $ g 0 0

emit' :: MonadState CodeState m => RInstructionGen -> m ()
emit' g = modify f
  where
    f CodeState {..} =
      CodeState
        { codeReversed = g : codeReversed,
          codeBinLength = codeBinLength + binLengthFromRInstruction g,
          ..
        }

emit :: MonadState CodeState m => RInstruction -> m ()
emit i = emit' $ \_ _ -> Right i

emitPool :: MonadState CodeState m => Word -> ByteString -> m Label
emitPool a bs = state f
  where
    f CodeState {..} =
      let offsetInPool' = align a offsetInPool
          o = builderRepeatZero $ fromIntegral $ offsetInPool' - offsetInPool
       in ( PoolRef offsetInPool',
            CodeState
              { offsetInPool = fromIntegral (BSL.length bs) + offsetInPool',
                poolReversed = lazyByteString bs : o : poolReversed,
                ..
              }
          )

computeCodeBinaryLength :: MonadCatch m => StateT CodeState m () -> m Int
computeCodeBinaryLength m = do
  CodeState {..} <- execStateT m (CodeState 0 [] [] [] 0)
  let poolOffset = instructionSize * fromIntegral (P.length codeReversed)
      poolOffsetAligned = align 8 poolOffset

      f :: (RInstructionGen, CodeOffset) -> Either String RInstruction
      f (ff, n) = ff n poolOffsetAligned

  code <- $eitherAddContext' $ mapM f $ P.zip (P.reverse codeReversed) (fmap (instructionSize *) [CodeOffset 0 ..])

  let txt = mconcat $ fmap (makeCodeBuilder . getInstruction) code
  P.return $ fromIntegral $ BSL.length txt

-- label :: MonadState CodeState m => m Label
-- label = gets (CodeRef . (* instructionSize) . fromIntegral . P.length . codeReversed)

label :: (MonadState CodeState m) => m Label
label = gets (CodeRef . fromIntegral . codeBinLength)

isPower2 :: (Bits i, Num i) => i -> Bool
isPower2 n = n .&. (n - 1) == 0

align :: Word -> CodeOffset -> CodeOffset
align a _ | not (isPower2 a) = error "align is not a power of 2"
align 0 n = n
align a n = (n + a' - 1) .&. complement (a' - 1)
  where
    a' = fromIntegral a

builderRepeatZero :: Int -> Builder
builderRepeatZero n = mconcat $ P.replicate n (word8 0)

b64 :: forall w. SingRegisterWidthI w => RRegister w -> Word32
b64 _ = case singRegisterWidth @w of
  SX -> 1
  SW -> 0

-- | C6.2.187 MOV (wide immediate)
mov :: (MonadState CodeState m) => Register -> Word16 -> m ()
mov r imm =
  emit $
    RInstruction $ (encodeMovqRegImm r (fromIntegral imm)) -- updated for x64

-- | The number can be represented with bitN bits
isBitN :: (Num b, Bits b, Ord b) => Int -> b -> Bool
isBitN bitN w =
  let m = complement $ (1 `shift` bitN) - 1
      h = w .&. m
   in if w >= 0 then h == 0 else h == m

findOffset :: CodeOffset -> Label -> CodeOffset
findOffset _poolOffset (CodeRef codeOffset) = codeOffset
findOffset poolOffset (PoolRef offsetInPool) = poolOffset + offsetInPool

ascii :: MonadState CodeState m => String -> m Label
ascii s = emitPool 1 $ BSLC.pack s

exportSymbol :: MonadState CodeState m => String -> Label -> m ()
exportSymbol s r = modify f
  where
    f (CodeState {..}) =
      CodeState
        { symbolsRefersed = (s, r) : symbolsRefersed,
          ..
        }

instructionSize :: Num b => b
instructionSize = 4

zeroIndexStringItem :: ElfSymbolXX 'ELFCLASS64
zeroIndexStringItem = ElfSymbolXX "" 0 0 0 0 0

textSecN, shstrtabSecN, strtabSecN, symtabSecN :: ElfSectionIndex
textSecN = 1
shstrtabSecN = 2
strtabSecN = 3
symtabSecN = 4

makeCodeBuilder :: Integer -> ByteString
makeCodeBuilder i = word8ArrayToByteString (reverseArray (integerToWord8Array i))

assemble :: MonadCatch m => StateT CodeState m () -> m Elf
assemble m = do
  CodeState {..} <- execStateT m (CodeState 0 [] [] [] 0)

  -- resolve txt

  let poolOffset = instructionSize * fromIntegral (P.length codeReversed)
      poolOffsetAligned = align 8 poolOffset

      f :: (RInstructionGen, CodeOffset) -> Either String RInstruction
      f (ff, n) = ff n poolOffsetAligned

  code <- $eitherAddContext' $ mapM f $ P.zip (P.reverse codeReversed) (fmap (instructionSize *) [CodeOffset 0 ..])

  let txt = mconcat $ fmap (makeCodeBuilder . getInstruction) code

  let ff :: (String, Label) -> ElfSymbolXX 'ELFCLASS64
      ff (s, r) =
        let steName = s
            steBind = STB_Global
            steType = STT_NoType
            steShNdx = textSecN
            steValue = fromIntegral $ findOffset poolOffset r
            steSize = 0 :: Word64
         in ElfSymbolXX {..}

      symbolTable = ff <$> P.reverse symbolsRefersed

  (symbolTableData, stringTableData) <- serializeSymbolTable ELFDATA2LSB (zeroIndexStringItem : symbolTable)

  trace ("") $
    P.return $
      Elf SELFCLASS64 $
        ElfHeader
          { ehData = ELFDATA2LSB, -- little endian
          -- {  ehData = ELFDATA2MSB, -- big endian
            ehOSABI = ELFOSABI_SYSV,
            ehABIVersion = 0,
            ehType = ET_REL,
            ehMachine = EM_X86_64,
            ehEntry = 0,
            ehFlags = 0
          }
          ~: ElfSection
            { esName = ".text",
              esType = SHT_PROGBITS,
              esFlags = SHF_EXECINSTR .|. SHF_ALLOC,
              esAddr = 0,
              esAddrAlign = 8,
              esEntSize = 0,
              esN = textSecN,
              esLink = 0,
              esInfo = 0,
              esData = ElfSectionData txt
            }
          ~: ElfSection
            { esName = ".shstrtab",
              esType = SHT_STRTAB,
              esFlags = 0,
              esAddr = 0,
              esAddrAlign = 1,
              esEntSize = 0,
              esN = shstrtabSecN,
              esLink = 0,
              esInfo = 0,
              esData = ElfSectionDataStringTable
            }
          ~: ElfSection
            { esName = ".symtab",
              esType = SHT_SYMTAB,
              esFlags = 0,
              esAddr = 0,
              esAddrAlign = 8,
              esEntSize = symbolTableEntrySize ELFCLASS64,
              esN = symtabSecN,
              esLink = fromIntegral strtabSecN,
              esInfo = 1,
              esData = ElfSectionData symbolTableData
            }
          ~: ElfSection
            { esName = ".strtab",
              esType = SHT_STRTAB,
              esFlags = 0,
              esAddr = 0,
              esAddrAlign = 1,
              esEntSize = 0,
              esN = strtabSecN,
              esLink = 0,
              esInfo = 0,
              esData = ElfSectionData stringTableData
            }
          ~: ElfSectionTable
          ~: ElfListNull

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- END OF DOCUMENTATION
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- OUR CODE NOW :)
------------------------------------------------------------------------------
------------------------------------------------------------------------------

integerToWord8Array :: Integer -> [Word8]
integerToWord8Array n = reverseArray $ go n []
  where
    go 0 acc = acc
    go m acc = go (m `shiftR` 8) (fromIntegral m : acc)

inverseEndiannessInteger :: Integer -> Integer
inverseEndiannessInteger i =
  let bs = integerToByteString i
      bs' = inverseEndiannessByteString bs
   in byteStringToInteger bs'

inverseEndiannessByteString :: ByteString -> ByteString
inverseEndiannessByteString bs =
  let w8array = BSL.unpack bs
      w8array' = inverseEndiannessW8Array w8array
   in BSL.pack w8array'

inverseEndiannessW8 :: Word8 -> Word8
inverseEndiannessW8 w =
  let w1 = w `shiftR` 4
      w2 = w .&. 0x0f
   in (w2 `shiftL` 4) .|. w1

inverseEndiannessW8Array :: [Word8] -> [Word8]
inverseEndiannessW8Array [] = []
inverseEndiannessW8Array (x : xs) = inverseEndiannessW8 x : inverseEndiannessW8Array xs

reverseArray :: [a] -> [a]
reverseArray [] = []
reverseArray (x : xs) = reverseArray xs ++ [x]

greatestPowerOf256 :: Integer -> Int
greatestPowerOf256 n = go n 0
  where
    go 0 acc = acc
    go m acc = go (m `shiftR` 8) (acc + 1)

integerToByteString :: Integer -> ByteString
integerToByteString n =
  BSL.pack
    ( [fromIntegral ((n `shiftR` (8 * i)) .&. 0xff) | i <- [0 .. greatestPowerOf256 n]]
    )

byteStringToInteger :: ByteString -> Integer
byteStringToInteger xs =
  let w8array = reverseArray (BSL.unpack xs)
   in P.foldr (\b acc -> acc * 256 + fromIntegral b) 0 w8array

byteListToStr :: [Int] -> String
byteListToStr [] = ""
byteListToStr (x : xs) = show x ++ byteListToStr xs

word32FromByteList :: [Int] -> Word32
word32FromByteList l = read (byteListToStr l) :: Word32

word8ArrayToByteString :: [Word8] -> ByteString
word8ArrayToByteString [] = BSL.pack []
word8ArrayToByteString (x : xs) = BSL.cons x (word8ArrayToByteString xs)

registerCode :: Register -> Word8
registerCode EAX = 0x00
registerCode ECX = 0x01
registerCode EDX = 0x02
registerCode EBX = 0x03
registerCode ESP = 0x04
registerCode EBP = 0x05
registerCode ESI = 0x06
registerCode EDI = 0x07
registerCode r = error ("unsupported register in mov instruction: " ++ show r)

movqOpCodeFromReg :: Register -> Word8
movqOpCodeFromReg EAX = 0xb8
movqOpCodeFromReg ECX = 0xb9
movqOpCodeFromReg EDX = 0xba
movqOpCodeFromReg EBX = 0xbb
movqOpCodeFromReg ESP = 0xbc
movqOpCodeFromReg EBP = 0xbd
movqOpCodeFromReg ESI = 0xbe
movqOpCodeFromReg EDI = 0xbf
movqOpCodeFromReg r = error ("unsupported register in mov instruction: " ++ show r)

movqRegImmByteArray :: Register -> Int -> [Word8]
movqRegImmByteArray r i =
  let opcode = movqOpCodeFromReg r
      immBytes = encodeImmediate i
   in [opcode] ++ immBytes

intToWord16 :: Int -> Word16
intToWord16 i = fromIntegral i :: Word16

encodeImmediate :: Int -> [Word8] -- Word32
encodeImmediate n
  | n >= 0 = [fromIntegral ((n `shiftR` (8 * i)) .&. 0xff) | i <- [0 .. 3]]
  | otherwise = [fromIntegral n]

convertOneInstruction :: MonadState CodeState m => Instruction -> m ()
convertOneInstruction (Mov (Reg r) (Immediate i)) = mov r (intToWord16 i)
convertOneInstruction (Mov (Reg r) (Memory i)) = encodeMovRegMem r i
convertOneInstruction (Mov (Memory i) (Reg r)) = encodeMovMemReg i r
convertOneInstruction (Mov (Reg r1) (Reg r2)) = encodeMovRegReg r1 r2
convertOneInstruction (Mov (Memory i) (Immediate imm)) = encodeMovMemImm i imm
convertOneInstruction (MovPtr (Memory i) (Immediate imm)) = encodeMovMemImm i imm
convertOneInstruction (MovPtr (Memory i) (Reg imm)) = encodeMovMemReg i imm
convertOneInstruction (MovStackAddr (Immediate ptr) (Immediate value)) = encodeMovStackAddrImm ptr value
convertOneInstruction (MovStackAddr (Immediate ptr) (Reg reg)) = encodeMovStackAddrReg ptr reg
convertOneInstruction (MovFromStackAddr (Reg reg) (Immediate ptr)) = encodeMovFromStackAddrReg reg ptr
convertOneInstruction (Push (Reg r)) = encodePushReg r
convertOneInstruction (Push (Immediate i)) = encodePushImm i
convertOneInstruction (Push (Memory i)) = encodePushMem i
convertOneInstruction (Pop (Reg r)) = encodePopReg r
convertOneInstruction (Pop (Memory i)) = encodePopMem i
convertOneInstruction (Xor (Reg r1) (Reg r2)) = encodeXorRegReg r1 r2
convertOneInstruction (Xor (Reg r1) (Immediate i)) = encodeXorRegImm r1 i
convertOneInstruction (Label name _) = encodeLabel name
convertOneInstruction (Add r1 (Reg r2)) = encodeAddRegReg r1 r2
convertOneInstruction (Add r1 (Immediate r2)) = encodeAddRegImm r1 r2
convertOneInstruction (Sub (Reg r1) (Reg r2)) = encodeSubRegReg r1 r2
convertOneInstruction (Sub (Reg r1) (Immediate r2)) = encodeSubRegImm r1 r2
convertOneInstruction (Inc r) = encodeIncReg r
convertOneInstruction (Dec r) = encodeDecReg r
convertOneInstruction (Neg r) = encodeNegReg r
convertOneInstruction (Div (Reg r)) = encodeDivReg r
convertOneInstruction (Mult (Reg r) (Reg r2)) = encodeMultReg r
convertOneInstruction i = error ("unsupported instruction: " ++ show i)

-- execAsm (Valid c) = execState (assemble (Rinstructions c)) (CodeState 0 [] [] [])

-------------------------------------------------------------------------------
-- Instructions opcodes
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- region MOV
-------------------------------------------------------------------------------

encodeMovrmandmrBasis :: (MonadState CodeState m) => Word8 -> Register -> Int -> m ()
encodeMovrmandmrBasis opcode r i =
  emit $
    RInstruction $
      ( byteStringToInteger
          ( word8ArrayToByteString
              ( [opcode] ++ (rr r) ++ [0x25] ++ encodeImmediate i
              )
          )
      )
  where
    rr :: Register -> [Word8]
    rr EAX = [0x04]
    rr ECX = [0x0c]
    rr EDX = [0x14]
    rr EBX = [0x1c]
    rr ESP = [0x24]
    rr EBP = [0x2c]
    rr ESI = [0x34]
    rr EDI = [0x3c]
    rr _ = error ("unsupported register in mov instruction: " ++ show r)

-- mov reg, mem
encodeMovRegMem :: (MonadState CodeState m) => Register -> Int -> m ()
encodeMovRegMem = encodeMovrmandmrBasis 0x8b

encodeMovMemReg :: (MonadState CodeState m) => Int -> Register -> m ()
encodeMovMemReg i r = encodeMovrmandmrBasis 0x89 r i

movOpcodeCombineRegReg :: Register -> Register -> Word8
movOpcodeCombineRegReg to from = (registerCode to .|. (registerCode from `shiftL` 3)) + (0xc0 :: Word8)

encodeMovRegReg :: (MonadState CodeState m) => Register -> Register -> m ()
encodeMovRegReg rto rfrom =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0x89, movOpcodeCombineRegReg rto rfrom]
        )

-- mov reg const
encodeMovqRegImm :: Register -> Int -> Integer
encodeMovqRegImm r i =
  byteStringToInteger
    (word8ArrayToByteString (movqRegImmByteArray r i))

-- mov mem, reg
-- ex: mov [42], dword 16
encodeMovMemImm :: (MonadState CodeState m) => Int -> Int -> m ()
encodeMovMemImm mem imm =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ( [0xc7, 0x04, 0x25] ++ encodeImmediate mem ++ encodeImmediate imm
            )
        )

-- mov [rsp + x], dword y -> movstackaddr x, y
encodeMovStackAddrImm :: (MonadState CodeState m) => Int -> Int -> m ()
encodeMovStackAddrImm mem imm =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ( [0xc7, 0x44, 0x24] ++ encodeImmediate mem ++ encodeImmediate imm
            )
        )

removeNullPrefix :: [Word8] -> [Word8]
removeNullPrefix [] = []
removeNullPrefix (x : xs) = if x == 0 then removeNullPrefix xs else x : xs

encodeMovStackAddrReg :: (MonadState CodeState m) => Int -> Register -> m ()
encodeMovStackAddrReg mem reg =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ( [0x89, rr reg, 0x24] ++ removeNullPrefix (reverseArray (encodeImmediate mem))
            )
        )
  where
    rr :: Register -> Word8
    rr EAX = 0x44
    rr ECX = 0x4c
    rr EDX = 0x54
    rr EBX = 0x5c
    rr ESP = 0x64
    rr EBP = 0x6c
    rr ESI = 0x74
    rr EDI = 0x7c
    rr r = error ("unsupported register in mov instruction: " ++ show r)

encodeMovFromStackAddrReg :: (MonadState CodeState m) => Register -> Int -> m ()
encodeMovFromStackAddrReg reg mem =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ( [0x8b, rr reg, 0x24] ++ removeNullPrefix (reverseArray (encodeImmediate mem))
            )
        )
  where
    rr :: Register -> Word8
    rr EAX = 0x44
    rr ECX = 0x4c
    rr EDX = 0x54
    rr EBX = 0x5c
    rr ESP = 0x64
    rr EBP = 0x6c
    rr ESI = 0x74
    rr EDI = 0x7c
    rr r = error ("unsupported register in mov instruction: " ++ show r)

-------------------------------------------------------------------------------
-- region Push
-------------------------------------------------------------------------------

encodePushReg :: (MonadState CodeState m) => Register -> m ()
encodePushReg r =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0x50 + registerCode r]
        )

encodePushImm :: (MonadState CodeState m) => Int -> m ()
encodePushImm imm =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            (0x6a : removeNullPrefix (reverseArray (encodeImmediate imm)))
        )

-- push [42]
-- push [register] is not used by our compiler
encodePushMem :: (MonadState CodeState m) => Int -> m ()
encodePushMem mem =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0xff, 0x34, 0x25] ++ encodeImmediate mem)
        )

-------------------------------------------------------------------------------
-- region Pop
-------------------------------------------------------------------------------

encodePopReg :: (MonadState CodeState m) => Register -> m ()
encodePopReg r =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0x58 + registerCode r]
        )

encodePopMem :: (MonadState CodeState m) => Int -> m ()
encodePopMem mem =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0x8f, 0x04, 0x25] ++ encodeImmediate mem)
        )

-------------------------------------------------------------------------------
-- region xor
-------------------------------------------------------------------------------

encodeXorRegReg :: (MonadState CodeState m) => Register -> Register -> m ()
encodeXorRegReg r1 r2 =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0x31, movOpcodeCombineRegReg r1 r2]
        )

encodeXorRegImm :: (MonadState CodeState m) => Register -> Int -> m ()
encodeXorRegImm r imm =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0x83, 0xf0 + registerCode r] ++ (removeNullPrefix (reverseArray (encodeImmediate imm))))
        )

-- we do not use other xor variants in the compiler

-------------------------------------------------------------------------------
-- region Label
-------------------------------------------------------------------------------

encodeLabel :: (MonadState CodeState m) => String -> m ()
encodeLabel name = do
  labelName <- label
  exportSymbol name labelName

-- add reg -> im ok and reg -> reg ok
-- Inc ok
-- Dec ok
-- Neg ok
-- Sub reg -> im ok and reg -> reg ok
-- Div  ok
-- Mult ok
-- jmp
-- Call
-- Interrupt
-- Alloc

-------------------------------------------------------------------------------
-- region Add
-------------------------------------------------------------------------------

encodeAddRegReg :: (MonadState CodeState m) => Register -> Register -> m ()
encodeAddRegReg r1 r2 =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0x01, movOpcodeCombineRegReg r1 r2]
        )

encodeAddRegImm :: (MonadState CodeState m) => Register -> Int -> m ()
encodeAddRegImm r imm =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0x83, 0xc0 + registerCode r] ++ removeNullPrefix (reverseArray (encodeImmediate imm)))
        )

-------------------------------------------------------------------------------
-- region Sub
-------------------------------------------------------------------------------

encodeSubRegReg :: (MonadState CodeState m) => Register -> Register -> m ()
encodeSubRegReg r1 r2 =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0x29, movOpcodeCombineRegReg r1 r2]
        )

encodeSubRegImm :: (MonadState CodeState m) => Register -> Int -> m ()
encodeSubRegImm r imm =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0x83, 0xe8 + registerCode r] ++ removeNullPrefix (reverseArray (encodeImmediate imm)))
        )

-------------------------------------------------------------------------------
-- region Inc
-------------------------------------------------------------------------------

encodeIncReg :: (MonadState CodeState m) => Register -> m ()
encodeIncReg r =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0xff, 0xc0 + registerCode r]
        )

-------------------------------------------------------------------------------
-- region Dec
-------------------------------------------------------------------------------

encodeDecReg :: (MonadState CodeState m) => Register -> m ()
encodeDecReg r =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0xff, 0xc8 + registerCode r]
        )

-------------------------------------------------------------------------------
-- region Neg
-------------------------------------------------------------------------------

encodeNegReg :: (MonadState CodeState m) => Register -> m ()
encodeNegReg r =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0xf7, 0xd8 + registerCode r]
        )

-------------------------------------------------------------------------------
-- region Div
-------------------------------------------------------------------------------

encodeDivReg :: (MonadState CodeState m) => Register -> m ()
encodeDivReg r =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0xf7, 0xf0 + registerCode r]
        )

-------------------------------------------------------------------------------
-- region Mult
-------------------------------------------------------------------------------

encodeMultReg :: (MonadState CodeState m) => Register -> m ()
encodeMultReg r =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0xf7, 0xe0 + registerCode r]
        )

-------------------------------------------------------------------------------
-- region Jmp
-------------------------------------------------------------------------------

-- findLabelForString :: String -> CodeState -> Maybe Label
-- findLabelForString s codeState =
--   case symbolsRefersed codeState of
--     [] -> Nothing
--     ((symbol, label) : rest) ->
--       if symbol == s
--         then Just label
--         else findLabelForString s (codeState {symbolsRefersed = rest})

-- -- Usage example:
-- -- Assuming 'codeState' is your CodeState
-- -- and 'symbol' is the string you want to look up
-- -- let labelForSymbol = findLabelForString symbol codeState

-- getPoolRef :: Maybe Label -> Maybe CodeOffset
-- getPoolRef maybeLabel = case maybeLabel of
--   Just (PoolRef offset) -> Just offset
--   _ -> Nothing

-- parseLabel :: MonadState CodeState m => String -> m [(String, Label)] -> Int64
-- parseLabel s [(str, label)]
--   | str == s = getCodeOffset label
--   | otherwise = parseLabel s []
-- parseLabel s [(str, label) : xs]
--   | str == s = getCodeOffset label
--   | otherwise = parseLabel s xs
-- parseLabel _ _ = 0

-- importSymbol :: MonadState CodeState m => String -> m CodeOffset
-- importSymbol s = res
--   where
--     f = gets (symbolsRefersed)
--     res = parseLabel s f

-- encodeJmp :: (MonadState CodeState m) => String -> m ()
-- encodeJmp name = do
--   labelName <- importSymbol name
--   case labelName of
--     Just label -> emit $ RInstruction $ byteStringToInteger (word8ArrayToByteString (0xe9 : encodeImmediate (fromIntegral label)))
--     Nothing -> error ("label not found: " ++ name)

-- emit $
--   RInstruction $
--     byteStringToInteger
--       ([0xeb] ++ [fromIntegral (findLabelForString labelName)])

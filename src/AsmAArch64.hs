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
    mov,
    ascii,
    label,
    exportSymbol,
    assemble,
    convertOneInstruction,
    encodeMovqRegImm,
    movqRegImmByteArray,
    byteStringToInteger,
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
import Data.Word
import Debug.Trace
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

-- instance Num ByteString where
--   (+) xs ys = BSL.pack (BSL.unpack xs ++ BSL.unpack ys)
--   (-) xs ys = BSL.pack (BSL.unpack xs ++ BSL.unpack ys)
--   (*) xs ys = BSL.pack (BSL.unpack xs ++ BSL.unpack ys)
--   negate xs = BSL.pack (BSL.unpack xs)
--   abs xs = BSL.pack (BSL.unpack xs)
--   signum xs = BSL.pack (BSL.unpack xs)
--   fromInteger n = BSL.pack (fromIntegral n :: [Word8])

-- byteStringToInteger :: ByteString -> Integer
-- byteStringToInteger xs = toInteger (BSL.foldr (\b acc -> acc * 256 + fromIntegral b) 0 xs)

-- instance Integral BS.ByteString where
--   toInteger xs = byteStringToInteger xs
--   quotRem xs ys = (BS.pack (fromIntegral q :: [Word8]), BS.pack (fromIntegral r :: [Word8]))
--     where
--       (q, r) = byteStringToInteger xs `quotRem` byteStringToInteger ys

-- instance Real ByteString where
--   toRational xs = toRational (BSL.foldr (\b acc -> acc * 256 + fromIntegral b) 0 (BSL.unpack xs))

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
    symbolsRefersed :: [(String, Label)]
  }

emit' :: MonadState CodeState m => RInstructionGen -> m ()
emit' g = modify f
  where
    f CodeState {..} =
      CodeState
        { codeReversed = g : codeReversed,
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

label :: MonadState CodeState m => m Label
label = gets (CodeRef . (* instructionSize) . fromIntegral . P.length . codeReversed)

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

-- makeCodeBuilder i = BSL.reverse (integerToByteString i)

assemble :: MonadCatch m => StateT CodeState m () -> m Elf
assemble m = do
  CodeState {..} <- execStateT m (CodeState 0 [] [] [])

  -- resolve txt

  let poolOffset = instructionSize * fromIntegral (P.length codeReversed)
      poolOffsetAligned = align 8 poolOffset

      f :: (RInstructionGen, CodeOffset) -> Either String RInstruction
      f (ff, n) = ff n poolOffsetAligned

  code <- $eitherAddContext' $ mapM f $ P.zip (P.reverse codeReversed) (fmap (instructionSize *) [CodeOffset 0 ..])

  --   let codeBuilder = mconcat $ fmap (word32LE . getInstruction) code
  --       txt =
  --         toLazyByteString $
  --           codeBuilder
  --             <> builderRepeatZero (fromIntegral $ poolOffsetAligned - poolOffset)
  --             <> mconcat (P.reverse poolReversed)

  --   let codeBuilder = mconcat $ fmap (makeCodeBuilder . getInstruction) code
  --       txt =
  --         toLazyByteString $
  --           codeBuilder
  -- <> builderRepeatZero (fromIntegral $ poolOffsetAligned - poolOffset)
  -- <> mconcat (P.reverse poolReversed)
  -- resolve symbolTable

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

  trace ("text is: " ++ show txt) $
    P.return $
      Elf SELFCLASS64 $
        ElfHeader
          { ehData = ELFDATA2LSB,
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

movqOpCodeFromReg :: Register -> Word8
movqOpCodeFromReg EAX = read "0xb8" :: Word8
movqOpCodeFromReg ECX = read "0xb9" :: Word8
movqOpCodeFromReg EDX = read "0xba" :: Word8
movqOpCodeFromReg EBX = read "0xbb" :: Word8
movqOpCodeFromReg ESP = read "0xbc" :: Word8
movqOpCodeFromReg EBP = read "0xbd" :: Word8
movqOpCodeFromReg ESI = read "0xbe" :: Word8
movqOpCodeFromReg EDI = read "0xbf" :: Word8

movqRegImmByteArray :: Register -> Int -> [Word8]
movqRegImmByteArray r i =
  let opcode = movqOpCodeFromReg r
      immBytes = encodeImmediate i
   in [opcode] ++ immBytes

encodeMovqRegImm :: Register -> Int -> Integer
encodeMovqRegImm r i =
  byteStringToInteger
    (word8ArrayToByteString ((movqRegImmByteArray r i)))

intToWord16 :: Int -> Word16
intToWord16 i = fromIntegral i :: Word16

encodeImmediate :: Int -> [Word8] -- Word32
encodeImmediate n =
  [fromIntegral ((n `shiftR` (8 * i)) .&. 0xff) | i <- [0 .. 3]]

convertOneInstruction :: MonadState CodeState m => Instruction -> m ()
convertOneInstruction (Mov (Reg r) (Immediate i)) = mov r (intToWord16 i)

hasmToAsm :: MonadState CodeState m => [Instruction] -> m [Int]
hasmToAsm [] = P.return []
hasmToAsm (i : is) =
  let op = convertOneInstruction i
   in do
        op
        hasmToAsm is

execAsm :: ValidState Context -> Elf
execAsm (Invalid s) = error s

-- execAsm (Valid c) = execState (assemble (Rinstructions c)) (CodeState 0 [] [] [])

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
{-# LANGUAGE BlockArguments #-}

module AsmAArch64
  ( CodeState (..),
    Register,
    Label,
    RInstructionGen,
    RInstruction (..),
    CodeOffset (..),
    RInstruction (..),
    emptyUnresolvedCall,
    emptyUnresolvedJmps,
    emit,
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
    word16Update2ndByte,
    compileInFile,
    compileInFileWrapper,
    elfOFile
  )
where

import Numeric (showHex)
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
import Data.Map
import DummyLd
import Control.Monad
import Data.Elf.PrettyPrint (printElf, printElf_, readFileLazy)

data RegisterWidth = X | W

data SingRegisterWidth :: RegisterWidth -> Type where
  SX :: SingRegisterWidth 'X
  SW :: SingRegisterWidth 'W

class SingRegisterWidthI (c :: RegisterWidth) where
  singRegisterWidth :: SingRegisterWidth c

newtype RRegister (c :: RegisterWidth) = R Word32

newtype CodeOffset = CodeOffset {getCodeOffset :: Int64} deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits)

newtype RInstruction = RInstruction {getInstruction :: Integer} deriving (Eq, Show, Num)

data Label
  = CodeRef !CodeOffset
  | PoolRef !CodeOffset

type RInstructionGen = CodeOffset -> CodeOffset -> Either String RInstruction

data CodeState = CodeState
  { offsetInPool :: CodeOffset,
    poolReversed :: [Builder],
    codeReversed :: [RInstructionGen],
    symbolsRefersed :: [(String, Label)],
    codeBinLength :: Int,
    unresolvedJmps :: Data.Map.Map String [(CodeOffset, Int)], 
    unresolvedCall :: Data.Map.Map String [(CodeOffset, Int)], 
    dataSec :: [Word8],
    dataRef :: [(String, Int)]
  }

emptyUnresolvedJmps :: Data.Map.Map String [(CodeOffset, Int)]
emptyUnresolvedJmps = Data.Map.empty

addUnresolvedJmp :: MonadState CodeState m => String -> CodeOffset -> m ()
addUnresolvedJmp name offset = do
  CodeState {..} <- get
  let codelen = P.length codeReversed
  let newMap = Data.Map.insertWith (++) name [(offset, codelen)] unresolvedJmps 
  put $ CodeState offsetInPool poolReversed codeReversed symbolsRefersed codeBinLength newMap unresolvedCall dataSec dataRef

getUnresolvedJmps :: MonadState CodeState m => String -> m [(CodeOffset, Int)]
getUnresolvedJmps name = do
  CodeState {..} <- get
  let maybeList = Data.Map.lookup name unresolvedJmps
  case maybeList of
    Just l -> P.return l
    Nothing -> P.return []

clearUnresolvedJmps :: MonadState CodeState m => String -> m ()
clearUnresolvedJmps name = do
  CodeState {..} <- get
  let newMap = Data.Map.delete name unresolvedJmps
  put $ CodeState offsetInPool poolReversed codeReversed symbolsRefersed codeBinLength newMap unresolvedCall dataSec dataRef

emptyUnresolvedCall :: Data.Map.Map String [(CodeOffset, Int)]
emptyUnresolvedCall = Data.Map.empty

addUnresolveCall :: MonadState CodeState m => String -> CodeOffset -> m ()
addUnresolveCall name offset = do
  CodeState {..} <- get
  let codelen = P.length codeReversed
  let newMap = Data.Map.insertWith (++) name [(offset, codelen)] unresolvedCall
  put $ CodeState offsetInPool poolReversed codeReversed symbolsRefersed codeBinLength unresolvedJmps newMap dataSec dataRef

getUnresolvedCall :: MonadState CodeState m => String -> m [(CodeOffset, Int)]
getUnresolvedCall name = do
  CodeState {..} <- get
  let maybeList = Data.Map.lookup name unresolvedCall
  case maybeList of
    Just l -> P.return l
    Nothing -> P.return []

clearUnresolvedCall :: MonadState CodeState m => String -> m ()
clearUnresolvedCall name = do
  CodeState {..} <- get
  let newMap = Data.Map.delete name unresolvedCall
  put $ CodeState offsetInPool poolReversed codeReversed symbolsRefersed codeBinLength unresolvedJmps newMap dataSec dataRef

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

findOffset :: CodeOffset -> Label -> CodeOffset
findOffset _poolOffset (CodeRef codeOffset) = codeOffset
findOffset poolOffset (PoolRef offsetInPool) = poolOffset + offsetInPool

ascii :: MonadState CodeState m => String -> m Label
ascii s = emitPool 1 $ BSLC.pack s

exportSymbol :: MonadState CodeState m => String -> Label -> m ()
exportSymbol s r = modify f
  where
    f CodeState {..} =
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

calcSize :: ByteString -> Int
calcSize bs = (P.length (BSL.unpack bs))

assemble :: MonadCatch m => StateT CodeState m () -> m Elf
assemble m = do
  CodeState {..} <- execStateT m (CodeState 0 [] [] [] 0 emptyUnresolvedJmps emptyUnresolvedCall [] [])

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

      ff' :: (String, Int) -> ElfSymbolXX 'ELFCLASS64
      ff' (s, r) =
        let steName = s
            steBind = STB_Global
            steType = STT_NoType
            steShNdx = textSecN
            steValue = fromIntegral $ r + (calcSize txt)
            steSize = 0 :: Word64
         in ElfSymbolXX {..}


      symbolTable = ff <$> P.reverse symbolsRefersed
      symbolTable' = symbolTable ++ (ff' <$> dataRef)

  (symbolTableData, stringTableData) <- serializeSymbolTable ELFDATA2LSB (zeroIndexStringItem : symbolTable')

  let dataVal = BSL.pack dataSec

  trace "" $
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
              esFlags = SHF_EXECINSTR .|. SHF_ALLOC .|. SHF_WRITE,
              esAddr = 0,
              esAddrAlign = 16,
              esEntSize = 0,
              esN = textSecN,
              esLink = 0,
              esInfo = 0,
              esData = ElfSectionData (txt `mappend` dataVal)
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

reverseArray :: [a] -> [a]
reverseArray [] = []
reverseArray (x : xs) = reverseArray xs ++ [x]

byteStringToInteger :: ByteString -> Integer
byteStringToInteger xs =
  let w8array = reverseArray (BSL.unpack xs)
   in P.foldr (\b acc -> acc * 256 + fromIntegral b) 0 w8array

word8ArrayToByteString :: [Word8] -> ByteString
word8ArrayToByteString xs = P.foldr BSL.cons (BSL.pack []) xs

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
   in opcode : immBytes

encodeImmediate :: Int -> [Word8]
encodeImmediate n
  | n >= 0 = [fromIntegral (n `shiftR` (8 * i) .&. 0xff) | i <- [0 .. 3]]
  | otherwise = [fromIntegral n]

easyPrintElf :: Elf -> IO ()
easyPrintElf elf = do
  doc <- printElf_ False elf
  print doc

writeElf :: FilePath -> Elf -> IO ()
writeElf path elf = do
    e <- serializeElf elf
    easyPrintElf elf
    BSL.writeFile path e

paramsRegisters = [EDI, ESI, EDX, ECX]

passParamsToStack :: (MonadState CodeState m) => [a] -> m ()
passParamsToStack argTypes = let params = reverseArray (P.zip paramsRegisters argTypes) in
    mapM_ (\(reg, _) -> encodePushReg reg ) params

blockToElf :: MonadCatch m => Block -> StateT CodeState m ()
blockToElf (Block name ctx argtypes) =
    case ctx of
        Invalid s -> error s
        Valid c -> do
            lbl <- label
            exportSymbol name lbl
            encodeEnter
            passParamsToStack argtypes
            contextToElfNoSelfRec c name
            encodeLeave
            encodeRetReg

blocksToElf :: MonadCatch m => BlockMap -> StateT CodeState m ()
blocksToElf bm = mapM_ (blockToElf . snd) (Data.Map.toList (blockMap bm))

declareStart :: MonadState CodeState m => m ()
declareStart = do
    lbl <- label
    exportSymbol "_start" lbl

filterBlocksNoSelfRec :: BlockMap -> String -> BlockMap
filterBlocksNoSelfRec bm name = BlockMap { blockMap = Data.Map.filterWithKey (\k _ -> k /= name) (blockMap bm) }

contextToElfNoSelfRec :: MonadCatch m => Context -> String -> StateT CodeState m ()
contextToElfNoSelfRec c name = do
    blocksToElf (filterBlocksNoSelfRec (blocks c) name)
    createElf (instructions c)

setupConstants :: MonadState CodeState m => m ()
setupConstants = do
    labelFormatInt <- emitPool 0 $ BSLC.pack "%d"
    exportSymbol "formatInt" labelFormatInt

    labelFormatBoolTrue <- emitPool 0 $ BSLC.pack "%s"
    exportSymbol "formatBoolTrue" labelFormatBoolTrue

    labelFormatBoolFalse <- emitPool 0 $ BSLC.pack "%s"
    exportSymbol "formatBoolFalse" labelFormatBoolFalse

contextToElf :: MonadCatch m => Context -> Bool -> StateT CodeState m ()
contextToElf c isExec = do
    setupConstants
    blocksToElf (blocks c)
    when isExec declareStart
    createElf (instructions c)
    when isExec appendAutoExit

createElf :: MonadCatch m => [Instruction] -> StateT CodeState m ()
createElf = mapM_ convertOneInstruction

appendAutoExit :: MonadState CodeState m => m ()
appendAutoExit = mapM_ convertOneInstruction [
    Xor (Reg EBX) (Reg EBX),
    Mov (Reg EBX) (Reg EAX),
    Mov (Reg EAX) (Immediate 1),
    Interrupt]

extractEntryAddress :: MonadCatch m => StateT CodeState m () -> m Int
extractEntryAddress st = do
    CodeState {..} <- execStateT st (CodeState 0 [] [] [] 0 emptyUnresolvedJmps emptyUnresolvedCall [] [])
    let labels = symbolsRefersed
    let maybeStart = Data.List.find (\(s, _) -> s == "_start") labels
    case maybeStart of
        Just (_, l) -> case l of
            CodeRef offset -> P.return (fromIntegral (getCodeOffset offset) :: Int)
            PoolRef _ -> error "start symbol is in the pool"
        Nothing -> error "no start symbol found"

elfExe :: MonadCatch m => Context -> m Elf
elfExe c = let
    elf = contextToElf c True
    in do
        stAddr <- extractEntryAddress elf
        assemble elf P.>>= dummyLd stAddr

elfOFile :: MonadCatch m => Context -> m Elf
elfOFile c = let
    elf = contextToElf c True
    in do
        assemble elf

compileInFileWrapper :: ValidState Context -> String -> Bool -> IO ()
compileInFileWrapper (Invalid s) _ _ = P.return ()
compileInFileWrapper (Valid c) name exec = compileInFile c name exec

compileInFile :: Context -> String -> Bool -> IO ()
compileInFile c name exec =
  if exec
    then elfExe c P.>>=  writeElf name
    else elfOFile c P.>>=  writeElf name

convertOneInstruction :: MonadState CodeState m => Instruction -> m ()
convertOneInstruction (Mov (Reg r) (Immediate i)) = encodeMovRegImm r i
convertOneInstruction (Mov (Reg r1) (Reg r2)) = encodeMovRegReg r1 r2
convertOneInstruction (Mov (Memory i) (Immediate imm)) = encodeMovMemImm i imm
convertOneInstruction (MovPtr (Memory i) (Immediate imm)) = encodeMovMemImm i imm
convertOneInstruction (MovPtr (Reg i) (Immediate imm)) = encodeMovRegImm i imm
convertOneInstruction (MovPtr (Reg i) (Reg imm)) = encodeMovRegReg i imm
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
convertOneInstruction (Mult (Reg r) (Reg r2)) = encodeMultReg r2
convertOneInstruction (Jmp name) = encodeJmp name
convertOneInstruction (Ret) = encodeRetReg
convertOneInstruction (Interrupt) = encodeInterrupt
convertOneInstruction (Or (Reg r1) (Reg r2)) = encodeOrRegReg r1 r2
convertOneInstruction (Or (Reg r1) (Immediate i)) = encodeOrRegImm r1 i
convertOneInstruction (And (Reg r1) (Reg r2)) = encodeAndRegReg r1 r2
convertOneInstruction (And (Reg r1) (Immediate i)) = encodeAndRegImm r1 i
convertOneInstruction (Not (Reg r)) = encodeNotReg r
convertOneInstruction (Cmp (Reg r1) (Reg r2)) = encodeCmpRegReg r1 r2
convertOneInstruction (Cmp (Reg r1) (Immediate i)) = encodeCmpRegImm r1 i
convertOneInstruction (Enter) = encodeEnter
convertOneInstruction (Leave) = encodeLeave
convertOneInstruction (Call name) = encodeCall name
convertOneInstruction (Alloc i) = encodeAlloc i
convertOneInstruction (Write fd (Immediate buf) len) = encodeWriteString fd (show buf) len
convertOneInstruction (Write fd (Symbol buf) len) = encodeWriteString fd buf len
convertOneInstruction i = allJmps i


-------------------------------------------------------------------------------
-- Instructions opcodes
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Instructions Write
-------------------------------------------------------------------------------

addDataString :: MonadState CodeState m => String -> m Int
addDataString str = do
  CodeState {..} <- get
  let addr = fromIntegral $ P.length dataSec
  let newData = dataSec ++ P.map (fromIntegral . fromEnum) str
  let newDataRef = dataRef ++ [("msg" ++ (show str), addr)]
  put $ CodeState offsetInPool poolReversed codeReversed symbolsRefersed codeBinLength unresolvedJmps unresolvedCall newData newDataRef
  P.return addr

write_str_in_data :: MonadState CodeState m =>  String -> m Int
write_str_in_data str = do
  let addr = addDataString str
  addr

encodeWriteString :: MonadState CodeState m => Int -> String -> Int -> m ()
encodeWriteString fd buf len = do
  addr <- write_str_in_data buf
  encodePushReg EDX
  encodePushReg ECX
  encodePushReg EBX
  encodePushReg EAX
  encodeMovRegImm EDX (P.length buf)
  encodeMovRegImm ECX ( 0x401000 + 0x34 + addr)
  encodeMovRegImm EBX fd
  encodeMovRegImm EAX 4
  encodeInterrupt
  encodePopReg EAX
  encodePopReg EBX
  encodePopReg ECX
  encodePopReg EDX
  
-------------------------------------------------------------------------------
-- region Call
-------------------------------------------------------------------------------

delayResolutionCall :: (MonadState CodeState m) => String -> m Int
delayResolutionCall name = do
    binLen <- gets codeBinLength
    addUnresolveCall name (CodeOffset . fromIntegral $ binLen)
    P.return (-1)

findAddrFromLabelCall :: (MonadState CodeState m) => String -> [(String, Label)] -> m Int
findAddrFromLabelCall name [] = delayResolutionCall name
findAddrFromLabelCall name ((str, lbl) : xs)
  | str == name = case lbl of
    CodeRef offset -> P.return $ fromIntegral offset
    PoolRef offset -> P.return $ fromIntegral offset
  | otherwise = findAddrFromLabelCall name xs

labelNameToAddrCall :: MonadState CodeState m => String -> m Int
labelNameToAddrCall name = do
    symbols <- getSymbols
    findAddrFromLabelCall name symbols


rightFill0xff :: [Word8] -> [Word8]
rightFill0xff xs = xs ++ P.replicate (5 - P.length xs) 0xff

rightFill0x00 :: [Word8] -> [Word8]
rightFill0x00 xs = xs ++ P.replicate (5 - P.length xs) 0x00

encodeCall :: MonadState CodeState m => String -> m ()
encodeCall name = do
    addr <- labelNameToAddrCall name
    binLen <- gets codeBinLength
    if addr == -1
        then
          do
            let delta = 47 - (binLen + 5)
            emit $ RInstruction $ byteStringToInteger . word8ArrayToByteString $ rightFill0x00 (0xe8 : (removeNullPrefix . reverseArray . encodeImmediate $ delta))

        else do
            let delta = addr - (binLen + 5)
            emit $ RInstruction $ byteStringToInteger . word8ArrayToByteString $ rightFill0xff (0xe8 : (removeNullPrefix . reverseArray . encodeImmediate $ delta))

-------------------------------------------------------------------------------
-- region MOV
-------------------------------------------------------------------------------

fillRightByte :: [Word8] -> Word8 -> Int -> [Word8]
fillRightByte fillme fillWith n = fillme ++ P.replicate (n - P.length fillme) fillWith

encodeMovRegImm :: (MonadState CodeState m) => Register -> Int -> m ()
encodeMovRegImm r i =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ( [0xb8 + registerCode r] ++ reverseArray (fillRightByte (reverseArray (encodeImmediate i)) 0 4)
            )
        )

movOpcodeCombineRegReg :: Register -> Register -> Word8
movOpcodeCombineRegReg to from = (registerCode to .|. registerCode from `shiftL` 3) + (0xc0 :: Word8)

encodeMovRegReg :: (MonadState CodeState m) => Register -> Register -> m ()
encodeMovRegReg rto rfrom =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0x89, movOpcodeCombineRegReg rto rfrom]
        )

encodeMovqRegImm :: Register -> Int -> Integer
encodeMovqRegImm r i =
  byteStringToInteger
    (word8ArrayToByteString (movqRegImmByteArray r i))

encodeMovMemImm :: (MonadState CodeState m) => Int -> Int -> m ()
encodeMovMemImm mem imm =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ( [0xc7, 0x04, 0x25] ++ encodeImmediate mem ++ encodeImmediate imm
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
            ( [0x89, rr reg] ++ removeNullPrefix (reverseArray (encodeImmediate (-(1 + mem) * 8))))
        )
  where
    rr :: Register -> Word8
    rr EAX = 0x45
    rr ECX = 0x4d
    rr EDX = 0x55
    rr EBX = 0x5d
    rr ESP = 0x65
    rr EBP = 0x6d
    rr ESI = 0x75
    rr EDI = 0x7d
    rr r = error ("Unsuported registers in mov instruction: " ++ show r)

encodeMovFromStackAddrReg :: (MonadState CodeState m) => Register -> Int -> m ()
encodeMovFromStackAddrReg reg mem =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0x8b, rr reg] ++ removeNullPrefix (reverseArray (encodeImmediate (-(1 + mem) * 8))))
        )
        where
            rr :: Register -> Word8
            rr EAX = 0x45
            rr ECX = 0x4d
            rr EDX = 0x55
            rr EBX = 0x5d
            rr ESP = 0x65
            rr EBP = 0x6d
            rr ESI = 0x75
            rr EDI = 0x7d
            rr r = error ("Unsuported registers in mov instruction: " ++ show r)

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
            (0x6a : is)
        )
    where
        is = if imm == 0 then [0x0] else removeNullPrefix (reverseArray (encodeImmediate imm))

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
            ([0x83, 0xf0 + registerCode r] ++ removeNullPrefix (reverseArray (encodeImmediate imm)))
        )


-------------------------------------------------------------------------------
-- region Label
-------------------------------------------------------------------------------

setElt :: Int -> a -> [a] -> [a]
setElt _ _ [] = []
setElt 0 x (_ : xs) = x : xs
setElt n x (y : xs) = y : setElt (n - 1) x xs

word16Update2ndByte :: Integer -> Word8 -> Integer 
word16Update2ndByte i w = if (i .&. 0x00ff) == 0 then i else
    (i .&. 0xff00) .|. (fromIntegral w :: Integer)

word16Update2ndByteCall :: Integer -> Word8 -> Integer 
word16Update2ndByteCall i w = (i .&. 0xff00000000) .|. (fromIntegral w `shiftL` 24 :: Integer)

updateJmpInstr :: (MonadState CodeState m) => Int -> Int -> RInstructionGen -> Bool -> m ()
updateJmpInstr instrIndex jmpDelta instrGen call = do
    case instrGen 0 0 of
        Left err -> error err
        Right instr -> do
            let instrBin = getInstruction instr
            let instrBin' = if call
                  then word16Update2ndByteCall instrBin (fromIntegral jmpDelta)
                  else word16Update2ndByte instrBin (fromIntegral jmpDelta)
            let instr' = (\_ _ -> Right (RInstruction instrBin'))
            modify (\s -> s {codeReversed = setElt instrIndex instr' (codeReversed s)})

hexLength :: Integer -> Int
hexLength n = P.length hexStr `div` 2 - 1
  where hexStr = showHex n ""

findLabelOffset :: [RInstructionGen] -> Int
findLabelOffset [] = 0
findLabelOffset (x : xs) = case x 0 0 of
    Left err -> error err
    Right instr -> (hexLength (getInstruction instr)) + findLabelOffset xs

resolveJmps :: (MonadState CodeState m) => CodeOffset -> [(CodeOffset, Int)] -> Bool -> m ()
resolveJmps labelAddr [] bool = P.return ()
resolveJmps labelAddr (x : xs) bool = do
    impl x bool
    resolveJmps labelAddr xs bool
    where
        impl :: (MonadState CodeState m) => (CodeOffset, Int) -> Bool -> m ()
        impl (jmpOffset, jmpInstrIndex') call = do
            code <- gets codeReversed
            let delta = if call
                        then ((findLabelOffset code) - jmpInstrIndex' - 1) 
                        else (fromIntegral labelAddr - fromIntegral jmpOffset - 2) :: Int

            let jmpInstrIndex = (P.length code - 1 - jmpInstrIndex')
            let instrGen = code !! jmpInstrIndex
            updateJmpInstr jmpInstrIndex delta instrGen call

encodeLabel :: (MonadState CodeState m) => String -> m ()
encodeLabel name = do
  l <- label
  let labelAddr = case l of
        CodeRef offset -> offset
        PoolRef offset -> offset
  exportSymbol name l
  unresolved <- getUnresolvedJmps name
  resolveJmps labelAddr unresolved False
  clearUnresolvedJmps name
  unresolvedC <- getUnresolvedCall name
  resolveJmps labelAddr unresolvedC True
  clearUnresolvedCall name

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
-- region Ret
-------------------------------------------------------------------------------

encodeRetReg :: (MonadState CodeState m) => m ()
encodeRetReg =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0xc3]
        )

-------------------------------------------------------------------------------
-- region Interrupt
-------------------------------------------------------------------------------

encodeInterrupt :: (MonadState CodeState m) => m ()
encodeInterrupt =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0xcd, 0x80]
        )

-------------------------------------------------------------------------------
-- region Or
-------------------------------------------------------------------------------

encodeOrRegReg :: (MonadState CodeState m) => Register -> Register -> m ()
encodeOrRegReg r1 r2 =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0x09, movOpcodeCombineRegReg r1 r2]
        )

encodeOrRegImm :: (MonadState CodeState m) => Register -> Int -> m ()
encodeOrRegImm r imm =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0x83, 0xc8 + registerCode r] ++ removeNullPrefix (reverseArray (encodeImmediate imm)))
        )

-------------------------------------------------------------------------------
-- region And
-------------------------------------------------------------------------------

encodeAndRegReg :: (MonadState CodeState m) => Register -> Register -> m ()
encodeAndRegReg r1 r2 =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0x21, movOpcodeCombineRegReg r1 r2]
        )

encodeAndRegImm :: (MonadState CodeState m) => Register -> Int -> m ()
encodeAndRegImm r imm =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0x83, 0xe0 + registerCode r] ++ removeNullPrefix (reverseArray (encodeImmediate imm)))
        )

-------------------------------------------------------------------------------
-- region Not
-------------------------------------------------------------------------------

encodeNotReg :: (MonadState CodeState m) => Register -> m ()
encodeNotReg r =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0xf7, 0xd0 + registerCode r]
        )

-------------------------------------------------------------------------------
-- region Cmp
-------------------------------------------------------------------------------

encodeCmpRegReg :: (MonadState CodeState m) => Register -> Register -> m ()
encodeCmpRegReg r1 r2 =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            [0x39, movOpcodeCombineRegReg r1 r2]
        )

encodeCmpRegImm :: (MonadState CodeState m) => Register -> Int -> m ()
encodeCmpRegImm r imm =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0x83, 0xf8 + registerCode r] ++ removeNullPrefix (reverseArray (encodeImmediate imm)))
        )

-------------------------------------------------------------------------------
-- region Enter leave
-------------------------------------------------------------------------------

encodeEnter :: (MonadState CodeState m) => m ()
encodeEnter =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0xc8, 0x00, 0x00, 0x00])
        )

encodeLeave :: (MonadState CodeState m) => m ()
encodeLeave =
  emit $
    RInstruction $
      byteStringToInteger
        ( word8ArrayToByteString
            ([0xc9])
        )

-------------------------------------------------------------------------------
-- region Je
-------------------------------------------------------------------------------

emptyJmp :: (MonadState CodeState m) => String -> Word8 -> m ()
emptyJmp name opcode = do
  addr <- labelNameToAddr name
  binLen <- gets codeBinLength
  let delta = addr - (binLen + 2)
  emit $
    RInstruction $
        byteStringToInteger . word8ArrayToByteString $
             opcode : (removeNullPrefix . reverseArray . encodeImmediate $ delta)

allJmps :: (MonadState CodeState m) => Instruction -> m ()
allJmps (Je name) = emptyJmp name 0x74
allJmps (Jne name) = emptyJmp name 0x75
allJmps (Js name) = emptyJmp name 0x78
allJmps (Jns name) = emptyJmp name 0x79
allJmps (Jg name) = emptyJmp name 0x7f
allJmps (Jge name) = emptyJmp name 0x7d
allJmps (Jl name) = emptyJmp name 0x7c
allJmps (Jle name) = emptyJmp name 0x7e
allJmps (Ja name) = emptyJmp name 0x77
allJmps (Jae name) = emptyJmp name 0x73
allJmps (Jb name) = emptyJmp name 0x72
allJmps (Jbe name) = emptyJmp name 0x76
allJmps i = error ("unsupported instruction: " ++ show i)



-------------------------------------------------------------------------------
-- region Alloc
-------------------------------------------------------------------------------

encodeAlloc :: (MonadState CodeState m) => Int -> m ()
encodeAlloc i = do
  encodePushReg EAX
  encodePushReg EDI
  encodeMovRegImm EDI i
  encodeMovRegImm EAX 45
  encodeInterrupt
  encodePopReg EDI
  encodePopReg EAX

-------------------------------------------------------------------------------
-- region Jmp
-------------------------------------------------------------------------------

delayResolution :: (MonadState CodeState m) => String -> m Int
delayResolution name = do
    binLen <- gets codeBinLength
    addUnresolvedJmp name (CodeOffset . fromIntegral $ binLen)
    P.return 42

findAddrFromLabel :: (MonadState CodeState m) => String -> [(String, Label)] -> m Int
findAddrFromLabel name [] = delayResolution name
findAddrFromLabel name ((str, lbl) : xs)
  | str == name = case lbl of
    CodeRef offset -> P.return $ fromIntegral offset
    PoolRef offset -> P.return $ fromIntegral offset
  | otherwise = findAddrFromLabel name xs

getSymbols :: MonadState CodeState m => m [(String, Label)]
getSymbols = gets symbolsRefersed

labelNameToAddr :: MonadState CodeState m => String -> m Int
labelNameToAddr name = do
    symbols <- getSymbols
    findAddrFromLabel name symbols

encodeJmp :: (MonadState CodeState m) => String -> m ()
encodeJmp name = do
    addr <- labelNameToAddr name
    binLen <- gets codeBinLength
    let delta = addr - (binLen + 2)
    emit $
      RInstruction $
        byteStringToInteger . word8ArrayToByteString $
          0xeb : (removeNullPrefix . reverseArray . encodeImmediate $ delta)

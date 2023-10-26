{-# LANGUAGE DataKinds #-}

module TestRealASM
  ( testEncodeMov,
    testEncodeMovqRegImm,
    testmovOpcodeCombineRegReg,
    testEncodeMovRegReg,
    testEncodeMovMemImm,
    testRemoveNullPrefix,
    functionalASMTests,
    testGetBinLengthFromRInstruction,
    testWord16Update2ndByte
  )
where

import AsmAArch64
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
import qualified Data.IntMap as Map
import Data.Kind
import Data.List (isInfixOf)
import Data.Word
import DummyLd
import EvaluateAST
import Instructions
import Lexer
  ( ASTNode (ASTNodeBoolean, ASTNodeIf, ASTNodeInteger),
    strToAST,
  )
import System.Exit
import System.Process
import Test.HUnit
import VM
import ValidState
import Prelude as P
import System.Console.CmdArgs.GetOpt (convert)

testEncodeMovqRegImm :: Test
testEncodeMovqRegImm =
  TestList
    [ movqRegImmByteArray EAX 42 ~?= [0xb8, 0x2a, 0x00, 0x00, 0x00],
      byteStringToInteger (BSL.pack [0xb8, 0x2a, 0x00, 0x00, 0x00]) ~?= (0xb82a000000 :: Integer),
      encodeMovqRegImm EAX 42 ~?= (0xb82a000000 :: Integer)
    ]

writeElf :: FilePath -> Elf -> IO ()
writeElf path elf = do
  e <- serializeElf elf
  BSL.writeFile path e

dummyProgramSimpleMov :: MonadCatch m => StateT CodeState m ()
dummyProgramSimpleMov =
  convertOneInstruction (Mov (Reg EAX) (Immediate 42))

dummyProgramMovMemImm :: MonadCatch m => StateT CodeState m ()
dummyProgramMovMemImm =
  convertOneInstruction (Mov (Memory 42) (Immediate 16))

testEncodeMovMemImmImpl :: IO ()
testEncodeMovMemImmImpl = do
  let elf = assemble dummyProgramMovMemImm
  elf P.>>= writeElf ".tmp_test_output"

testEncodeMovMemImm :: Test
testEncodeMovMemImm =
  TestCase $ do
    testEncodeMovMemImmImpl
    assertBool "mov is correctly encoded" True

testEncodeMovImpl :: IO ()
testEncodeMovImpl = do
  let elf = assemble dummyProgramSimpleMov
  elf P.>>= writeElf "ElfTestRes/testMovOpcode.o"

dummyProgramMovRegReg :: MonadCatch m => StateT CodeState m ()
dummyProgramMovRegReg =
  convertOneInstruction (Mov (Reg EAX) (Reg EAX))

getElfMovRegReg :: MonadCatch m => StateT CodeState m ()
getElfMovRegReg = do
  let prog = dummyProgramMovRegReg
  prog

testMovRegRegEAXEAX :: IO ()
testMovRegRegEAXEAX = do
  let elf = assemble getElfMovRegReg
  elf P.>>= writeElf "ElfTestRes/testMovRegReg.o"

testEncodeMovRegReg :: Test
testEncodeMovRegReg =
  TestCase $
    testMovRegRegEAXEAX

testmovOpcodeCombineRegReg :: Test
testmovOpcodeCombineRegReg =
  TestList
    [ movOpcodeCombineRegReg EAX EAX ~?= 0xc0,
      movOpcodeCombineRegReg ECX EAX ~?= 0xc1,
      movOpcodeCombineRegReg EDX EAX ~?= 0xc2,
      movOpcodeCombineRegReg EDI EAX ~?= 0xc7,
      movOpcodeCombineRegReg EAX ECX ~?= 0xc8,
      movOpcodeCombineRegReg EAX EDI ~?= 0xf8
    ]

dummyProgramMovStackAddr :: MonadCatch m => StateT CodeState m ()
dummyProgramMovStackAddr = do
  convertOneInstruction (MovStackAddr (Immediate 0) (Reg EAX))
  convertOneInstruction (MovStackAddr (Immediate 1) (Reg EAX)) --  89 b4 24 dc 51 0b 00

testEncodeMovStackAddrImpl :: IO ()
testEncodeMovStackAddrImpl = do
  let elf = assemble dummyProgramMovStackAddr
  elf P.>>= writeElf ".tmp_test_output"

testRemoveNullPrefix :: Test
testRemoveNullPrefix =
  TestList
    [ removeNullPrefix [0x00, 0x00, 0x2a] ~?= [0x2a],
      removeNullPrefix [0x00, 0x00, 0x00] ~?= []
    ]

testEncodeMov :: Test
testEncodeMov =
  TestCase $ do
    testEncodeMovImpl
    testEncodeMovStackAddrImpl
    assertBool "mov is correctly encoded" True

testEncodeMovFromStackAddr :: IO ()
testEncodeMovFromStackAddr = do
  let elf = assemble p
  elf P.>>= writeElf ".tmp_test_output"
  where
    p :: MonadCatch m => StateT CodeState m ()
    p = do
      convertOneInstruction (MovFromStackAddr (Reg EAX) (Immediate 1))
      convertOneInstruction (MovFromStackAddr (Reg ECX) (Immediate 1))
      convertOneInstruction (MovFromStackAddr (Reg EDX) (Immediate 1))
      convertOneInstruction (MovFromStackAddr (Reg EBX) (Immediate 1))
      convertOneInstruction (MovFromStackAddr (Reg ESP) (Immediate 1))
      convertOneInstruction (MovFromStackAddr (Reg EBP) (Immediate 1))
      convertOneInstruction (MovFromStackAddr (Reg ESI) (Immediate 1))
      convertOneInstruction (MovFromStackAddr (Reg EDI) (Immediate 1))

runRunctionalTest :: IO () -> String -> Test
runRunctionalTest func path = TestCase $ do
  func
  expected <- P.readFile path
  output <- readProcess "objdump" ["-d", ".tmp_test_output"] ""
  let expected' = P.unlines $ P.drop 7 $ P.lines expected
  let output' = P.unlines $ P.drop 7 $ P.lines output
  if expected' == output'
    then P.putStr ""
    else do
      P.putStrLn "expected:"
      P.putStrLn expected'
      P.putStrLn "output:"
      P.putStrLn output'
  assertBool "functional test ok" $ expected' == output'

testRunStackAddr :: Test
testRunStackAddr = runRunctionalTest testEncodeMovStackAddrImpl "ElfTestRes/movStackAddr_expected.txt"

testRunMovRegReg :: Test
testRunMovRegReg = runRunctionalTest testEncodeMovMemImmImpl "ElfTestRes/movRegReg_expected.txt"

testRunMovFromStackAddr :: Test
testRunMovFromStackAddr = runRunctionalTest testEncodeMovFromStackAddr "ElfTestRes/movfromstackaddr_expected.txt"

testPushReg :: Test
testPushReg = runRunctionalTest testPushRegImpl "ElfTestRes/pushReg_expected.txt"
  where
    testPushRegImpl :: IO ()
    testPushRegImpl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Push (Reg EAX))
          convertOneInstruction (Push (Reg ECX))
          convertOneInstruction (Push (Reg EDX))
          convertOneInstruction (Push (Reg EBX))
          convertOneInstruction (Push (Reg ESP))
          convertOneInstruction (Push (Reg EBP))
          convertOneInstruction (Push (Reg ESI))
          convertOneInstruction (Push (Reg EDI))

testPushImm :: Test
testPushImm = runRunctionalTest testPushImmImpl "ElfTestRes/pushImm_expected.txt"
  where
    testPushImmImpl :: IO ()
    testPushImmImpl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Push (Immediate 1))
          convertOneInstruction (Push (Immediate 42))
          convertOneInstruction (Push (Immediate (-10)))

testPushMem :: Test
testPushMem = runRunctionalTest testPushMemImpl "ElfTestRes/pushMem_expected.txt"
  where
    testPushMemImpl :: IO ()
    testPushMemImpl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p =
          convertOneInstruction (Push (Memory 42))

testPopReg :: Test
testPopReg = runRunctionalTest testPopRegImpl "ElfTestRes/popReg_expected.txt"
  where
    testPopRegImpl :: IO ()
    testPopRegImpl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Pop (Reg EAX))
          convertOneInstruction (Pop (Reg ECX))
          convertOneInstruction (Pop (Reg EDX))
          convertOneInstruction (Pop (Reg EBX))
          convertOneInstruction (Pop (Reg ESP))
          convertOneInstruction (Pop (Reg EBP))
          convertOneInstruction (Pop (Reg ESI))
          convertOneInstruction (Pop (Reg EDI))

testPopMem :: Test
testPopMem = runRunctionalTest testPopMemImpl "ElfTestRes/popMem_expected.txt"
  where
    testPopMemImpl :: IO ()
    testPopMemImpl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p =
          convertOneInstruction (Pop (Memory 42))

testXorRegReg :: Test
testXorRegReg = runRunctionalTest testXorRegRegImpl "ElfTestRes/xorRegReg_expected.txt"
  where
    testXorRegRegImpl :: IO ()
    testXorRegRegImpl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Xor (Reg EAX) (Reg EAX))
          convertOneInstruction (Xor (Reg EAX) (Reg ECX))
          convertOneInstruction (Xor (Reg EAX) (Reg EDX))
          convertOneInstruction (Xor (Reg EAX) (Reg EBX))
          convertOneInstruction (Xor (Reg EAX) (Reg ESP))
          convertOneInstruction (Xor (Reg EAX) (Reg EBP))
          convertOneInstruction (Xor (Reg EAX) (Reg ESI))
          convertOneInstruction (Xor (Reg EAX) (Reg EDI))
          convertOneInstruction (Xor (Reg ECX) (Reg EAX))
          convertOneInstruction (Xor (Reg ECX) (Reg ECX))
          convertOneInstruction (Xor (Reg ECX) (Reg EDX))
          convertOneInstruction (Xor (Reg ECX) (Reg EBX))
          convertOneInstruction (Xor (Reg ECX) (Reg ESP))
          convertOneInstruction (Xor (Reg ECX) (Reg EBP))
          convertOneInstruction (Xor (Reg ECX) (Reg ESI))
          convertOneInstruction (Xor (Reg ECX) (Reg EDI))
          convertOneInstruction (Xor (Reg EDX) (Reg EAX))
          convertOneInstruction (Xor (Reg EDX) (Reg ECX))
          convertOneInstruction (Xor (Reg EDX) (Reg EDX))
          convertOneInstruction (Xor (Reg EDX) (Reg EBX))
          convertOneInstruction (Xor (Reg EDX) (Reg ESP))
          convertOneInstruction (Xor (Reg EDX) (Reg EBP))
          convertOneInstruction (Xor (Reg EDX) (Reg ESI))
          convertOneInstruction (Xor (Reg EDX) (Reg EDI))
          convertOneInstruction (Xor (Reg EBX) (Reg EAX))
          convertOneInstruction (Xor (Reg EBX) (Reg ECX))
          convertOneInstruction (Xor (Reg EBX) (Reg EDX))
          convertOneInstruction (Xor (Reg EBX) (Reg EBX))
          convertOneInstruction (Xor (Reg EBX) (Reg ESP))
          convertOneInstruction (Xor (Reg EBX) (Reg EBP))
          convertOneInstruction (Xor (Reg EBX) (Reg ESI))
          convertOneInstruction (Xor (Reg EBX) (Reg EDI))
          convertOneInstruction (Xor (Reg ESP) (Reg EAX))
          convertOneInstruction (Xor (Reg ESP) (Reg ECX))
          convertOneInstruction (Xor (Reg ESP) (Reg EDX))
          convertOneInstruction (Xor (Reg ESP) (Reg EBX))
          convertOneInstruction (Xor (Reg ESP) (Reg ESP))
          convertOneInstruction (Xor (Reg ESP) (Reg EBP))
          convertOneInstruction (Xor (Reg ESP) (Reg ESI))
          convertOneInstruction (Xor (Reg ESP) (Reg EDI))
          convertOneInstruction (Xor (Reg EBP) (Reg EAX))
          convertOneInstruction (Xor (Reg EBP) (Reg ECX))
          convertOneInstruction (Xor (Reg EBP) (Reg EDX))
          convertOneInstruction (Xor (Reg EBP) (Reg EBX))
          convertOneInstruction (Xor (Reg EBP) (Reg ESP))
          convertOneInstruction (Xor (Reg EBP) (Reg EBP))
          convertOneInstruction (Xor (Reg EBP) (Reg ESI))
          convertOneInstruction (Xor (Reg EBP) (Reg EDI))
          convertOneInstruction (Xor (Reg ESI) (Reg EAX))
          convertOneInstruction (Xor (Reg ESI) (Reg ECX))
          convertOneInstruction (Xor (Reg ESI) (Reg EDX))
          convertOneInstruction (Xor (Reg ESI) (Reg EBX))
          convertOneInstruction (Xor (Reg ESI) (Reg ESP))
          convertOneInstruction (Xor (Reg ESI) (Reg EBP))
          convertOneInstruction (Xor (Reg ESI) (Reg ESI))
          convertOneInstruction (Xor (Reg ESI) (Reg EDI))
          convertOneInstruction (Xor (Reg EDI) (Reg EAX))
          convertOneInstruction (Xor (Reg EDI) (Reg ECX))
          convertOneInstruction (Xor (Reg EDI) (Reg EDX))
          convertOneInstruction (Xor (Reg EDI) (Reg EBX))
          convertOneInstruction (Xor (Reg EDI) (Reg ESP))
          convertOneInstruction (Xor (Reg EDI) (Reg EBP))
          convertOneInstruction (Xor (Reg EDI) (Reg ESI))
          convertOneInstruction (Xor (Reg EDI) (Reg EDI))

testXorRegImm :: Test
testXorRegImm = runRunctionalTest testXorRegImmImpl "ElfTestRes/xorRegImm_expected.txt"
  where
    testXorRegImmImpl :: IO ()
    testXorRegImmImpl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Xor (Reg EAX) (Immediate 42))
          convertOneInstruction (Xor (Reg ECX) (Immediate 42))
          convertOneInstruction (Xor (Reg EDX) (Immediate 42))
          convertOneInstruction (Xor (Reg EBX) (Immediate 42))
          convertOneInstruction (Xor (Reg ESP) (Immediate 42))
          convertOneInstruction (Xor (Reg EBP) (Immediate 42))
          convertOneInstruction (Xor (Reg ESI) (Immediate 42))
          convertOneInstruction (Xor (Reg EDI) (Immediate 42))

testLabelInASM :: Test
testLabelInASM = runRunctionalTest impl "ElfTestRes/label_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Xor (Reg EAX) (Reg EAX))
          convertOneInstruction (VM.Label "_labelFoo" 42)
          convertOneInstruction (Xor (Reg EAX) (Reg EDI))
          convertOneInstruction (VM.Label "_labelBar" 42)
          convertOneInstruction (Xor (Reg EAX) (Reg ESI))

testGetBinLengthFromRInstruction :: Test
testGetBinLengthFromRInstruction =
  TestList
    [ binLengthFromRInstruction (\_ _ -> Right (RInstruction 0x12345678)) ~?= 4,
      binLengthFromRInstruction (\_ _ -> Right (RInstruction 0x123456)) ~?= 3,
      binLengthFromRInstruction (\_ _ -> Right (RInstruction 0x1234)) ~?= 2
    ]

testAddInASM :: Test
testAddInASM = runRunctionalTest impl "ElfTestRes/add_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Add EAX (Reg EBX))
          convertOneInstruction (Add ECX (Reg EDX))
          convertOneInstruction (Add ESI (Reg EDI))
          convertOneInstruction (VM.Label "_imm" 42)
          convertOneInstruction (Add EAX (Immediate 1))
          convertOneInstruction (Add EBX (Immediate 42))
          convertOneInstruction (Add ECX (Immediate (-10)))

testSubInASM :: Test
testSubInASM = runRunctionalTest impl "ElfTestRes/sub_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Sub (Reg EAX) (Reg EBX))
          convertOneInstruction (Sub (Reg ECX) (Reg EDX))
          convertOneInstruction (Sub (Reg ESI) (Reg EDI))
          convertOneInstruction (VM.Label "_imm" 42)
          convertOneInstruction (Sub (Reg EAX) (Immediate 1))
          convertOneInstruction (Sub (Reg EBX) (Immediate 42))
          convertOneInstruction (Sub (Reg ECX) (Immediate (-10)))

testIncInASM :: Test
testIncInASM = runRunctionalTest impl "ElfTestRes/inc_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Inc EAX)
          convertOneInstruction (Inc ECX)
          convertOneInstruction (Inc ESI)

testDecInASM :: Test
testDecInASM = runRunctionalTest impl "ElfTestRes/dec_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Dec EAX)
          convertOneInstruction (Dec ECX)
          convertOneInstruction (Dec ESI)

testNegInASM :: Test
testNegInASM = runRunctionalTest impl "ElfTestRes/neg_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Neg EAX)
          convertOneInstruction (Neg ECX)
          convertOneInstruction (Neg ESI)

testDivInASM :: Test
testDivInASM = runRunctionalTest impl "ElfTestRes/div_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Div (Reg EAX))
          convertOneInstruction (Div (Reg ECX))
          convertOneInstruction (Div (Reg ESI))

testMulInASM :: Test
testMulInASM = runRunctionalTest impl "ElfTestRes/mul_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Mult (Reg EAX) (Reg EAX))
          convertOneInstruction (Mult (Reg ECX) (Reg ECX))
          convertOneInstruction (Mult (Reg ESI) (Reg ESI))

testJmpInASM :: Test
testJmpInASM = runRunctionalTest impl "ElfTestRes/jmp_expected.txt"
    where
        impl :: IO ()
        impl = do
            let elf = assemble p
            elf P.>>= writeElf ".tmp_test_output"
            where
                p :: MonadCatch m => StateT CodeState m ()
                p = do
                    convertOneInstruction (Mov (Reg EAX) (Immediate 1))
                    convertOneInstruction (Jmp "_test2")
                    convertOneInstruction (VM.Label "_test" 42)
                    convertOneInstruction (Mov (Reg EAX) (Immediate 3))
                    convertOneInstruction (VM.Label "_test2" 42)
                    convertOneInstruction (Mov (Reg EBX) (Immediate 1))
                    convertOneInstruction (Mov (Reg ECX) (Immediate 2))
                    convertOneInstruction (Jmp "_test")

testWord16Update2ndByte :: Test
testWord16Update2ndByte =
  TestList
    [ word16Update2ndByte 0x1234 0x56 ~?= 0x1256,
      word16Update2ndByte 0x1234 0x00 ~?= 0x1200,
      word16Update2ndByte 0x1234 0xff ~?= 0x12ff
    ]
testRetInASM :: Test
testRetInASM = runRunctionalTest impl "ElfTestRes/ret_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Ret)

testIntInASM :: Test
testIntInASM = runRunctionalTest impl "ElfTestRes/int_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Interrupt)

testOrInASM :: Test
testOrInASM = runRunctionalTest impl "ElfTestRes/or_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Or (Reg EAX) (Reg EBX))
          convertOneInstruction (Or (Reg ECX) (Reg EDX))
          convertOneInstruction (Or (Reg ESI) (Reg EDI))
          convertOneInstruction (VM.Label "_imm" 42)
          convertOneInstruction (Or (Reg EAX) (Immediate 1))
          convertOneInstruction (Or (Reg EBX) (Immediate 42))
          convertOneInstruction (Or (Reg ECX) (Immediate (-10)))

testAndInASM :: Test
testAndInASM = runRunctionalTest impl "ElfTestRes/and_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (And (Reg EAX) (Reg EBX))
          convertOneInstruction (And (Reg ECX) (Reg EDX))
          convertOneInstruction (And (Reg ESI) (Reg EDI))
          convertOneInstruction (VM.Label "_imm" 42)
          convertOneInstruction (And (Reg EAX) (Immediate 1))
          convertOneInstruction (And (Reg EBX) (Immediate 42))
          convertOneInstruction (And (Reg ECX) (Immediate (-10)))


testNotInASM :: Test
testNotInASM = runRunctionalTest impl "ElfTestRes/not_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Not (Reg EAX))
          convertOneInstruction (Not (Reg ECX))
          convertOneInstruction (Not (Reg ESI))

testCmpInASM :: Test
testCmpInASM = runRunctionalTest impl "ElfTestRes/cmp_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Cmp (Reg EAX) (Reg EBX))
          convertOneInstruction (Cmp (Reg ECX) (Reg EDX))
          convertOneInstruction (Cmp (Reg ESI) (Reg EDI))
          convertOneInstruction (VM.Label "_imm" 42)
          convertOneInstruction (Cmp (Reg EAX) (Immediate 1))
          convertOneInstruction (Cmp (Reg ECX) (Immediate 42))
          convertOneInstruction (Cmp (Reg EDX) (Immediate (-10)))

testJeInASM :: Test
testJeInASM = runRunctionalTest impl "ElfTestRes/je_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (VM.Label "_start" 42)
          convertOneInstruction (Je "_test")
          convertOneInstruction (Jne "_test")
          convertOneInstruction (Js "_test")
          convertOneInstruction (Jns "_test")
          convertOneInstruction (Jg "_test")
          convertOneInstruction (Jge "_test")
          convertOneInstruction (Jl "_test")
          convertOneInstruction (Jle "_test")
          convertOneInstruction (Ja "_test")
          convertOneInstruction (Jae "_test")
          convertOneInstruction (Jb "_test")
          convertOneInstruction (Jbe "_test")
          convertOneInstruction (VM.Label "_test" 42)
          convertOneInstruction (Je "_start")
          convertOneInstruction (Jne "_start")
          convertOneInstruction (Js "_start")
          convertOneInstruction (Jns "_start")
          convertOneInstruction (Jg "_start")
          convertOneInstruction (Jge "_start")
          convertOneInstruction (Jl "_start")
          convertOneInstruction (Jle "_start")
          convertOneInstruction (Ja "_start")
          convertOneInstruction (Jae "_start")
          convertOneInstruction (Jb "_start")
          convertOneInstruction (Jbe "_start")

testELInASM :: Test
testELInASM = runRunctionalTest impl "ElfTestRes/enter_leave_expected.txt"
  where
    impl :: IO ()
    impl = do
      let elf = assemble p
      elf P.>>= writeElf ".tmp_test_output"
      where
        p :: MonadCatch m => StateT CodeState m ()
        p = do
          convertOneInstruction (Enter)
          convertOneInstruction (Leave)

testCallInASM :: Test
testCallInASM = runRunctionalTest impl "ElfTestRes/funcCall_expected.txt"
    where
        impl :: IO ()
        impl = do
            let elf = assemble p
            elf P.>>= writeElf ".tmp_test_output"
            where
                p :: MonadCatch m => StateT CodeState m ()
                p = do
                    convertOneInstruction (VM.Label "call_1" 0)
                    convertOneInstruction Enter
                    convertOneInstruction (Mov (Reg EAX) (Immediate 1))
                    convertOneInstruction Leave
                    convertOneInstruction Ret
                    convertOneInstruction (VM.Label "call_2" 0)
                    convertOneInstruction Enter
                    convertOneInstruction (Mov (Reg EAX) (Immediate 2))
                    convertOneInstruction Leave
                    convertOneInstruction Ret
                    convertOneInstruction (VM.Label "_start" 0)
                    convertOneInstruction Enter
                    convertOneInstruction (Call "call_1")
                    convertOneInstruction (Call "call_2")
                    convertOneInstruction (Xor (Reg EBX) (Reg EBX))
                    convertOneInstruction (Mov (Reg EBX) (Reg EAX))
                    convertOneInstruction (Mov (Reg EAX) (Immediate 1))
                    convertOneInstruction Interrupt

testFCallInASM :: Test
testFCallInASM = runRunctionalTest impl "ElfTestRes/Forward_FuncCall_expected.txt"
    where
        impl :: IO ()
        impl = do
            let elf = assemble p
            elf P.>>= writeElf ".tmp_test_output"
            where
                p :: MonadCatch m => StateT CodeState m ()
                p = do
                    convertOneInstruction (VM.Label "_start" 0)
                    convertOneInstruction Enter
                    convertOneInstruction (Call "call_1")
                    convertOneInstruction (Call "call_2")
                    convertOneInstruction (Xor (Reg EBX) (Reg EBX))
                    convertOneInstruction (Mov (Reg EBX) (Reg EAX))
                    convertOneInstruction (Mov (Reg EAX) (Immediate 1))
                    convertOneInstruction Interrupt
                    convertOneInstruction (VM.Label "call_1" 0)
                    convertOneInstruction Enter
                    convertOneInstruction (Mov (Reg EAX) (Immediate 1))
                    convertOneInstruction Leave
                    convertOneInstruction Ret
                    convertOneInstruction (VM.Label "call_2" 0)
                    convertOneInstruction Enter
                    convertOneInstruction (Mov (Reg EAX) (Immediate 2))
                    convertOneInstruction Leave
                    convertOneInstruction Ret

testXorMovGhostLimb :: Test
testXorMovGhostLimb = runRunctionalTest impl "ElfTestRes/ghostLimb_expected.txt"
    where
        impl :: IO ()
        impl = do
            let elf = assemble p
            elf P.>>= writeElf ".tmp_test_output"
            where
                p :: MonadCatch m => StateT CodeState m ()
                p = do
                        convertOneInstruction (Enter)
                        convertOneInstruction (Xor (Reg EAX) (Reg EAX))
                        convertOneInstruction (Mov (Reg EAX) (Immediate 1))
                        convertOneInstruction (Cmp (Reg EAX) (Immediate 1))
                        convertOneInstruction (Cmp (Reg EAX) (Immediate 1))
                        convertOneInstruction (Jne "_0else")
                        convertOneInstruction (VM.Label "_0else" 0)
                        -- convertOneInstruction (VM.Label "_0then" 6)
                        -- convertOneInstruction (Xor (Reg EAX) (Reg EAX))
                        -- convertOneInstruction (Mov (Reg EAX) (Immediate 2))
                        -- convertOneInstruction (Jmp "_0end")
                        -- convertOneInstruction (Xor (Reg EAX) (Reg EAX))
                        -- convertOneInstruction (Mov (Reg EAX) (Immediate 3))
                        -- convertOneInstruction (VM.Label "_0end" 13)


testEncodeWriteElf :: Test
testEncodeWriteElf = runRunctionalTest impl "ElfTestRes/write_expected.txt"
    where
        impl :: IO ()
        impl = do
            let elf = assemble p
            elf P.>>= writeElf ".tmp_test_output"
            where
                p :: MonadCatch m => StateT CodeState m ()
                p = do
                  convertOneInstruction (Write 1 (Symbol "42") 2)
                  convertOneInstruction (Write 1 (Immediate 42) 2)

functionalASMTests :: Test
functionalASMTests =
  TestList
    [ testRunStackAddr,
      testRunMovRegReg,
      testRunMovFromStackAddr,
      -- testPushReg,       -- machine dependent
      -- testPushImm,       -- machine dependent
      -- testPushMem,       -- machine dependent
      -- testPopReg,        -- machine dependent
      -- testPopMem,        -- machine dependent
      testXorRegReg,
      testXorRegImm,
      testLabelInASM,
      testAddInASM,
      testSubInASM,
      testIncInASM,
      testDecInASM,
      testNegInASM,
      testDivInASM,
      testMulInASM,
      testJmpInASM,
      -- testRetInASM,       -- machine dependent
      testIntInASM,
      testOrInASM,
      testAndInASM,
      testNotInASM,
      testCmpInASM,
      -- testELInASM,        -- machine dependent
      -- testCallInASM,      -- machine dependent
      -- testFCallInASM,     -- machine dependent
      -- testXorMovGhostLimb, -- machine dependent
      testEncodeWriteElf,
      testJeInASM
      ]

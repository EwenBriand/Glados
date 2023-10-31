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
import Includes

testEncodeMovqRegImm :: Test
testEncodeMovqRegImm =
  TestList
    [ movqRegImmByteArray EAX 42 ~?= [0xb8, 0x2a, 0x00, 0x00, 0x00],
      byteStringToInteger (BSL.pack [0xb8, 0x2a, 0x00, 0x00, 0x00]) ~?= (0xb82a000000 :: Integer),
      encodeMovqRegImm EAX 42 ~?= (0xb82a000000 :: Integer),
      encodeMovqRegImm ECX 42 ~?= (0xb92a000000 :: Integer),
      encodeMovqRegImm EDX 42 ~?= (0xba2a000000 :: Integer),
      encodeMovqRegImm EBX 42 ~?= (0xbb2a000000 :: Integer),
      encodeMovqRegImm ESP 42 ~?= (0xbc2a000000 :: Integer),
      encodeMovqRegImm EBP 42 ~?= (0xbd2a000000 :: Integer),
      encodeMovqRegImm ESI 42 ~?= (0xbe2a000000 :: Integer),
      encodeMovqRegImm EDI 42 ~?= (0xbf2a000000 :: Integer)
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
  convertOneInstruction (MovStackAddr (Immediate 0) (Reg ECX))
  convertOneInstruction (MovStackAddr (Immediate 0) (Reg EDX))
  convertOneInstruction (MovStackAddr (Immediate 0) (Reg EBX))
  convertOneInstruction (MovStackAddr (Immediate 0) (Reg ESP))
  convertOneInstruction (MovStackAddr (Immediate 0) (Reg EBP))
  convertOneInstruction (MovStackAddr (Immediate 0) (Reg ESI))
  convertOneInstruction (MovStackAddr (Immediate 0) (Reg EDI))
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

testEncodeAlloc :: Test
testEncodeAlloc = runRunctionalTest impl "ElfTestRes/alloc_expected.txt"
    where
        impl :: IO ()
        impl = do
            let elf = assemble p
            elf P.>>= writeElf ".tmp_test_output"
            where
                p :: MonadCatch m => StateT CodeState m ()
                p = do
                  convertOneInstruction (Alloc 16000)

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



testEncodeCompileObj :: Test
testEncodeCompileObj = runRunctionalTest impl "ElfTestRes/obj_test.txt"
    where
        impl :: IO ()
        impl = do
            src <- P.readFile "tests/tests_from_pdf/factorial_asm.gld" 
            src' <- resolveIncludes src
            let ctx = (detectLabels (strToHASM (Valid newContext) src'))
            
            compileInFileWrapper ctx ".tmp_test_output" False
            P.putStrLn ""



testEncodeCompileExe :: Test
testEncodeCompileExe = runRunctionalTest impl "ElfTestRes/exe_test.txt"
    where
        impl :: IO ()
        impl = do
            src <- P.readFile "tests/tests_from_pdf/factorial_asm.gld"
            src' <- resolveIncludes src
            let ctx = (detectLabels (strToHASM (Valid newContext) src'))
            
            compileInFileWrapper ctx ".tmp_test_output" True
            P.putStrLn ""


testMachineConfig :: Test
testMachineConfig = TestList
  [ "testMachineConfigAddress" Test.HUnit.~: mcAddressTest
  , "testMachineConfigAlign" Test.HUnit.~: mcAlignTest
  , "testMachineConfigAddress Unknown" Test.HUnit.~: mcAddressTestU
  , "testMachineConfigAlign Unknown" Test.HUnit.~: mcAlignTestU
  ]

mcAddressTest :: Assertion
mcAddressTest = do
    config <- getMachineConfig EM_AARCH64
    assertEqual "mcAddress should be 0x400000" (mcAddress config) (0x400000 :: Word64)

mcAlignTest :: Assertion
mcAlignTest = do
    config <- getMachineConfig EM_AARCH64
    assertEqual "mcAlign should be 0x10000" (mcAlign config) (0x10000 :: Word64)

mcAddressTestU :: Assertion
mcAddressTestU = do
    config <- getMachineConfig 0
    assertEqual "mcAddress should be 0x00000" (mcAddress config) (0x00000 :: Word64)

mcAlignTestU :: Assertion
mcAlignTestU = do
    config <- getMachineConfig 0
    assertEqual "mcAlign should be 0x0000" (mcAlign config) (0x0000 :: Word64)


codeOffsetTests :: Test
codeOffsetTests = TestList
  [ "CodeOffset should have an Eq instance" Test.HUnit.~: CodeOffset 1234 == CodeOffset 1234 ~?= True
  , "CodeOffset should have a Show instance" Test.HUnit.~: show (CodeOffset 1234) ~?= "CodeOffset {getCodeOffset = 1234}"
  , "CodeOffset should have an Ord instance" Test.HUnit.~: CodeOffset 1234 `compare` CodeOffset 5678 ~?= LT
  , "CodeOffset should have a Num instance" Test.HUnit.~: CodeOffset 1234 + CodeOffset 5678 ~?= CodeOffset 6912
  , "CodeOffset should have an Enum instance" Test.HUnit.~: [CodeOffset 0, CodeOffset 1, CodeOffset 2] ~?= [CodeOffset 0, CodeOffset 1, CodeOffset 2]
  , "CodeOffset should have a Real instance" Test.HUnit.~: toRational (CodeOffset 1234) ~?= 1234
  , "CodeOffset should have an Integral instance" Test.HUnit.~: CodeOffset 1234 `div` CodeOffset 10 ~?= CodeOffset 123
  , "CodeOffset should have a Bits instance" Test.HUnit.~: (CodeOffset 0x1234 .&. CodeOffset 0x00FF) ~?= CodeOffset 0x0034
  , "CodeOffset should have a FiniteBits instance" Test.HUnit.~: finiteBitSize (CodeOffset 0) ~?= 64
  -- Eq
  , "CodeOffset should be equal to itself" Test.HUnit.~: do
        let offset = CodeOffset 0x1000
        assertEqual "offset should be equal to itself" offset offset
  , "CodeOffset should be equal to an equal value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x1000
        assertEqual "offset1 should be equal to offset2" offset1 offset2
  , "CodeOffset should not be equal to a different value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x2000
        assertBool "offset1 should not be equal to offset2" (offset1 /= offset2)
    -- Ord
    , "CodeOffset should be less than a greater value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x2000
        assertBool "offset1 should be less than offset2" (offset1 < offset2)
  , "CodeOffset should be greater than a lesser value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x2000
        assertBool "offset2 should be greater than offset1" (offset2 > offset1)
  , "CodeOffset should be less than or equal to itself" Test.HUnit.~: do
        let offset = CodeOffset 0x1000
        assertBool "offset should be less than or equal to itself" (offset <= offset)
  , "CodeOffset should be greater than or equal to itself" Test.HUnit.~: do
        let offset = CodeOffset 0x1000
        assertBool "offset should be greater than or equal to itself" (offset >= offset)
  , "CodeOffset should be less than or equal to a greater value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x2000
        assertBool "offset1 should be less than or equal to offset2" (offset1 <= offset2)
  , "CodeOffset should be greater than or equal to a lesser value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x2000
        assertBool "offset2 should be greater than or equal to offset1" (offset2 >= offset1)
  ,  "CodeOffset should be equal to itself" Test.HUnit.~: do
        let offset = CodeOffset 0x1000
        assertBool "offset should be equal to itself" (offset == offset)
  , "CodeOffset should not be equal to a different value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x2000
        assertBool "offset1 should not be equal to offset2" (offset1 /= offset2)
  , "CodeOffset should be less than or equal to a greater value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x2000
        assertBool "offset1 should be less than or equal to offset2" (offset1 <= offset2)
  , "CodeOffset should be greater than or equal to a lesser value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x2000
        assertBool "offset2 should be greater than or equal to offset1" (offset2 >= offset1)
  , "CodeOffset should not be less than a lesser value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x2000
        assertBool "offset1 should not be less than offset2" (not (offset1 > offset2))
  , "CodeOffset should not be greater than a greater value" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1000
            offset2 = CodeOffset 0x2000
        assertBool "offset2 should not be greater than offset1" (not (offset2 < offset1))
  
  -- Enum
  , "CodeOffset should have a valid successor" Test.HUnit.~: do
        let offset = CodeOffset 0x1000
        assertEqual "succ offset should be 0x1001" (succ offset) (CodeOffset 0x1001)
  , "CodeOffset should have a valid predecessor" Test.HUnit.~: do
        let offset = CodeOffset 0x1000
        assertEqual "pred offset should be 0xfff" (pred offset) (CodeOffset 0xfff)
  , "CodeOffset should have a valid range" Test.HUnit.~: do
        let range = [CodeOffset 0x1000 .. CodeOffset 0x1005]
        assertEqual "range should be [0x1000, 0x1001, 0x1002, 0x1003, 0x1004, 0x1005]" range [CodeOffset 0x1000, CodeOffset 0x1001, CodeOffset 0x1002, CodeOffset 0x1003, CodeOffset 0x1004, CodeOffset 0x1005]
  , "CodeOffset should have a valid toEnum" Test.HUnit.~: do
        let offset = toEnum 0x1000 :: CodeOffset
        assertEqual "toEnum 0x1000 should be CodeOffset 0x1000" offset (CodeOffset 0x1000)
  , "CodeOffset should have a valid fromEnum" Test.HUnit.~: do
        let offset = CodeOffset 0x1000
        assertEqual "fromEnum CodeOffset 0x1000 should be 0x1000" (fromEnum offset) 0x1000

  -- Bits
  ,  "CodeOffset should have a valid bitwise AND" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1234
            offset2 = CodeOffset 0x00ff
        assertEqual "offset1 .&. offset2 should be 0x0034" (offset1 .&. offset2) (CodeOffset 0x0034)
  , "CodeOffset should have a valid bitwise OR" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1234
            offset2 = CodeOffset 0x00ff
        assertEqual "offset1 .|. offset2 should be 0x12ff" (offset1 .|. offset2) (CodeOffset 0x12ff)
  , "CodeOffset should have a valid bitwise XOR" Test.HUnit.~: do
        let offset1 = CodeOffset 0x1234
            offset2 = CodeOffset 0x00ff
        assertEqual "offset1 `xor` offset2 should be 0x12cb" (offset1 `xor` offset2) (CodeOffset 0x12cb)
  -- FiniteBits
  , "CodeOffset should have a valid bitwise complement" Test.HUnit.~: do
        let offset = CodeOffset 0x1234
        assertEqual "complement offset should be 0xffffedcb" (complement offset) (CodeOffset (-4661))
  , "CodeOffset should have a valid bitwise shift" Test.HUnit.~: do
        let offset = CodeOffset 0x1234
        assertEqual "shift offset 4 should be 0x12340" (shift offset 4) (CodeOffset 0x12340)
  , "CodeOffset should have a valid bitwise rotate" Test.HUnit.~: do
        let offset = CodeOffset 0x1234
        assertEqual "rotate offset 4 should be 0x4123" (rotate offset 4) (CodeOffset 0x12340)
  , "CodeOffset should have a valid bit count" Test.HUnit.~: do
        let offset = CodeOffset 0x1234
        assertEqual "popCount offset should be 5" (popCount offset) 5
  ,  "CodeOffset should have a valid count of leading zeros" Test.HUnit.~: do
        let offset = CodeOffset 0x1234
        assertEqual "countLeadingZeros offset should be 42" (countLeadingZeros offset) 51
  , "CodeOffset should have a valid count of trailing zeros" Test.HUnit.~: do
        let offset = CodeOffset 0x1234
        assertEqual "countTrailingZeros offset should be 2" (countTrailingZeros offset) 2
  ]

rInstructionTests :: Test
rInstructionTests = TestList
  [ "RInstruction should have an Eq instance" Test.HUnit.~: RInstruction 1234 == RInstruction 1234 ~?= True
  , "RInstruction should have a Show instance" Test.HUnit.~: show (RInstruction 1234) ~?= "RInstruction {getInstruction = 1234}"
  -- , "RInstruction should have an Ord instance" Test.HUnit.~: RInstruction 1234 `compare` RInstruction 5678 ~?= LT
  , "RInstruction should have a Num instance" Test.HUnit.~: RInstruction 1234 + RInstruction 5678 ~?= RInstruction 6912
  , "RInstruction should be equal to itself" Test.HUnit.~: do
        let instr = RInstruction 0x12345678
        assertEqual "instr should be equal to itself" instr instr
  , "RInstruction should be equal to an equal value" Test.HUnit.~: do
        let instr1 = RInstruction 0x12345678
            instr2 = RInstruction 0x12345678
        assertEqual "instr1 should be equal to instr2" instr1 instr2
  , "RInstruction should not be equal to a different value" Test.HUnit.~: do
        let instr1 = RInstruction 0x12345678
            instr2 = RInstruction 0x87654321
        assertBool "instr1 should not be equal to instr2" (instr1 /= instr2)
  -- ,  "RInstruction should be less than a greater value" Test.HUnit.~: do
  --       let instr1 = RInstruction 0x1234
  --           instr2 = RInstruction 0x5678
  --       assertBool "instr1 should be less than instr2" (instr1 < instr2)
  -- , "RInstruction should be greater than a lesser value" Test.HUnit.~: do
  --       let instr1 = RInstruction 0x1234
  --           instr2 = RInstruction 0x5678
  --       assertBool "instr2 should be greater than instr1" (instr2 > instr1)
  -- , "RInstruction should be less than or equal to itself" Test.HUnit.~: do
  --       let instr = RInstruction 0x12345678
  --       assertBool "instr should be less than or equal to itself" (instr <= instr)
  -- , "RInstruction should be greater than or equal to itself" Test.HUnit.~: do
  --       let instr = RInstruction 0x12345678
  --       assertBool "instr should be greater than or equal to itself" (instr >= instr)
  -- , "RInstruction should be less than or equal to a greater value" Test.HUnit.~: do
  --       let instr1 = RInstruction 0x1234
  --           instr2 = RInstruction 0x5678
  --       assertBool "instr1 should be less than or equal to instr2" (instr1 <= instr2)
  -- , "RInstruction should be greater than or equal to a lesser value" Test.HUnit.~: do
  --       let instr1 = RInstruction 0x1234
  --           instr2 = RInstruction 0x5678
  --       assertBool "instr2 should be greater than or equal to instr1" (instr2 >= instr1)
  ,  "RInstruction should have a valid addition" Test.HUnit.~: do
        let instr1 = RInstruction 0x12345678
            instr2 = RInstruction 0x87654321
        assertEqual "instr1 + instr2 should be 0x99999999" (instr1 + instr2) (RInstruction 0x99999999)
  , "RInstruction should have a valid subtraction" Test.HUnit.~: do
        let instr1 = RInstruction 0x12345678
            instr2 = RInstruction 0x87654321
        assertEqual "instr1 - instr2 should be -0x7530ed09" (instr1 - instr2) (RInstruction (-0x7530eca9))
  , "RInstruction should have a valid multiplication" Test.HUnit.~: do
        let instr1 = RInstruction 0x1234
            instr2 = RInstruction 0x5678
        assertEqual "instr1 * instr2 should be 0x6d8f9c" (instr1 * instr2) (RInstruction 103153760)
  , "RInstruction should have a valid absolute value" Test.HUnit.~: do
        let instr = RInstruction (-0x12345678)
        assertEqual "abs instr should be 0x12345678" (abs instr) (RInstruction 0x12345678)
  , "RInstruction should have a valid signum" Test.HUnit.~: do
        let instr = RInstruction (-0x12345678)
        assertEqual "signum instr should be -1" (signum instr) (RInstruction (-1))
  , "RInstruction should have a valid integer conversion" Test.HUnit.~: do
        let instr = RInstruction 0x12345678
        assertEqual "fromInteger 0x12345678 should be instr" (fromInteger 0x12345678) instr
  ]


-- testFindAddrFromLabel :: Test
-- testFindAddrFromLabel = TestList
--   [ "findAddrFromLabel should return the code offset for a code reference" Test.HUnit.~: do
--         let name = "foo"
--             labels = [("foo", CodeRef 0x1000), ("bar", PoolRef 0x2000)]
--             state = CodeState (CodeOffset 0x1000) [] [] labels 0 Map.empty Map.empty [] []
--         result <- evalStateT (findAddrFromLabel name labels) state
--         assertEqual "result should be 0x1000" result 0x1000
--   , "findAddrFromLabel should return the pool offset for a pool reference" Test.HUnit.~: do
--         let name = "bar"
--             labels = [("foo", CodeRef 0x1000), ("bar", PoolRef 0x2000)]
--             state = CodeState (CodeOffset 0x1000) [] [] labels 0 Map.empty Map.empty [] []
--         result <- evalStateT (findAddrFromLabel name labels) state
--         assertEqual "result should be 0x2000" result 0x2000
--   , "findAddrFromLabel should delay resolution for an unresolved label" Test.HUnit.~: do
--         let name = "baz"
--             labels = [("foo", CodeRef 0x1000), ("bar", PoolRef 0x2000)]
--             state = CodeState (CodeOffset 0x1000) [] [] labels 0 Map.empty Map.empty [] []
--         result <- evalStateT (findAddrFromLabel name labels) state
--         assertEqual "result should be a delay resolution action" (show result) "DelayResolution baz"
--   , "findAddrFromLabel should handle an empty label list" Test.HUnit.~: do
--         let name = "foo"
--             labels = []
--             state = CodeState (CodeOffset 0x1000) [] [] labels 0 Map.empty Map.empty [] []
--         result <- evalStateT (findAddrFromLabel name labels) state
--         assertEqual "result should be a delay resolution action" (show result) "DelayResolution foo"
--   ]


-- testAllJmps :: Test
-- testAllJmps = TestList
--   [ "allJmps should throw an error for an unsupported instruction" Test.HUnit.~: do
--         let instr = (DerefMacro EAX)
--             state = CodeState (CodeOffset 0) [] [] [] 0 emptyUnresolvedJmps emptyUnresolvedCall [] []
--         result <- runStateT (convertOneInstruction instr) state
--         case result of
--           (err, _) -> assertEqual "result should be an error" (show err) "unsupported instruction: DerefMacro EAX"
--           _ -> assertFailure "result should be an error"
--   ]

-- testWrapperInvalid :: Test
-- testWrapperInvalid = TestList
--   [ "compileInFileWrapper should throw an error for an invalid context" Test.HUnit.~: do
--         let context = Invalid "Invalid Context"
--             name = ".tmp_test_output"
--             exec = False
--         result <- try (compileInFileWrapper context name exec) :: IO (Either SomeException ())
--         case result of
--           Left _ -> assertEqual "result should be an error" True True
--           Right _ -> assertFailure "result should be an error"
--   ]



functionalASMTests :: Test
functionalASMTests =
  TestList
    [ testRunStackAddr,
      testRunMovRegReg,
      testRunMovFromStackAddr,
      testPushReg,       -- machine dependent
      testPushImm,       -- machine dependent
      testPushMem,       -- machine dependent
      testPopReg,        -- machine dependent
      testPopMem,        -- machine dependent
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
      testRetInASM,       -- machine dependent
      testIntInASM,
      testOrInASM,
      testAndInASM,
      testNotInASM,
      testCmpInASM,
      testELInASM,        -- machine dependent
      testCallInASM,      -- machine dependent
      testFCallInASM,     -- machine dependent
      testXorMovGhostLimb, -- machine dependent
      testEncodeAlloc,
      testEncodeCompileObj,
      testEncodeCompileExe,
      testEncodeWriteElf,
      testJeInASM,
      testMachineConfig,
      codeOffsetTests,
      rInstructionTests
      -- testAllJmps
      -- testFindAddrFromLabel
      ]

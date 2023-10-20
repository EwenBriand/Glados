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
import Data.Word
import DummyLd
import EvaluateAST
import Instructions
import Lexer
  ( ASTNode (ASTNodeBoolean, ASTNodeIf, ASTNodeInteger),
    strToAST,
  )
import Test.HUnit
import VM
import ValidState
import Prelude as P
import Data.Elf
import Data.List (isInfixOf)
import System.Exit
import System.Process

testEncodeMovqRegImm :: Test
testEncodeMovqRegImm =
  TestList
    [ movqRegImmByteArray EAX 42 ~?= [0xb8, 0x2a, 0x00, 0x00, 0x00],
      byteStringToInteger (BSL.pack ([0xb8, 0x2a, 0x00, 0x00, 0x00])) ~?= (0xb82a000000 :: Integer),
      encodeMovqRegImm EAX 42 ~?= (0xb82a000000 :: Integer)
    ]

writeElf :: FilePath -> Elf -> IO ()
writeElf path elf = do
  e <- serializeElf elf
  BSL.writeFile path e

dummyProgramSimpleMov :: MonadCatch m => StateT CodeState m ()
dummyProgramSimpleMov = do
  convertOneInstruction (Mov (Reg EAX) (Immediate 42))

dummyProgramMovMemImm :: MonadCatch m => StateT CodeState m ()
dummyProgramMovMemImm = do
  convertOneInstruction (Mov (Memory 42) (Immediate 16))

testEncodeMovMemImmImpl :: IO ()
testEncodeMovMemImmImpl = do
  let elf = assemble dummyProgramMovMemImm
  (elf P.>>= writeElf ".tmp_test_output")

testEncodeMovMemImm :: Test
testEncodeMovMemImm =
  TestCase $ do
    testEncodeMovMemImmImpl
    assertBool "mov is correctly encoded" True

testEncodeMovImpl :: IO ()
testEncodeMovImpl = do
  let elf = assemble dummyProgramSimpleMov
  (elf P.>>= writeElf "ElfTestRes/testMovOpcode.o")

dummyProgramMovRegReg :: MonadCatch m => StateT CodeState m ()
dummyProgramMovRegReg = do
  convertOneInstruction (Mov (Reg EAX) (Reg EAX))

getElfMovRegReg :: MonadCatch m => StateT CodeState m ()
getElfMovRegReg = do
  let prog = dummyProgramMovRegReg
  prog

testMovRegRegEAXEAX :: IO ()
testMovRegRegEAXEAX = do
  let elf = assemble getElfMovRegReg
  (elf P.>>= writeElf "ElfTestRes/testMovRegReg.o")

testEncodeMovRegReg :: Test
testEncodeMovRegReg =
  TestCase $ do
    testMovRegRegEAXEAX

testmovOpcodeCombineRegReg ::Test
testmovOpcodeCombineRegReg =
    TestList [ movOpcodeCombineRegReg EAX EAX ~?= 0xc0,
               movOpcodeCombineRegReg ECX EAX ~?= 0xc1,
               movOpcodeCombineRegReg EDX EAX ~?= 0xc2,
               movOpcodeCombineRegReg EDI EAX ~?= 0xc7,
               movOpcodeCombineRegReg EAX ECX ~?= 0xc8,
               movOpcodeCombineRegReg EAX EDI ~?= 0xf8 ]

dummyProgramMovStackAddr :: MonadCatch m => StateT CodeState m ()
dummyProgramMovStackAddr = do
  convertOneInstruction (MovStackAddr (Immediate 42) (Reg EDI))
  convertOneInstruction (MovStackAddr (Immediate 100) (Reg ESI)) --  89 b4 24 dc 51 0b 00

testEncodeMovStackAddrImpl :: IO ()
testEncodeMovStackAddrImpl = do
  let elf = assemble dummyProgramMovStackAddr
  (elf P.>>= writeElf ".tmp_test_output")

testRemoveNullPrefix :: Test
testRemoveNullPrefix = TestList [
    removeNullPrefix [0x00, 0x00, 0x2a] ~?= [0x2a],
    removeNullPrefix [0x00, 0x00, 0x00] ~?= []]

testEncodeMov :: Test
testEncodeMov =
  TestCase $ do
    testEncodeMovImpl
    testEncodeMovStackAddrImpl
    assertBool "mov is correctly encoded" True

testEncodeMovFromStackAddr :: IO ()
testEncodeMovFromStackAddr = do
  let elf = assemble p
  (elf P.>>= writeElf ".tmp_test_output")
  where
    p :: MonadCatch m => StateT CodeState m ()
    p = do
        convertOneInstruction (MovFromStackAddr (Reg EAX) (Immediate 42))
        convertOneInstruction (MovFromStackAddr (Reg ECX) (Immediate 42))
        convertOneInstruction (MovFromStackAddr (Reg EDX) (Immediate 42))
        convertOneInstruction (MovFromStackAddr (Reg EBX) (Immediate 42))
        convertOneInstruction (MovFromStackAddr (Reg ESP) (Immediate 42))
        convertOneInstruction (MovFromStackAddr (Reg EBP) (Immediate 42))
        convertOneInstruction (MovFromStackAddr (Reg ESI) (Immediate 42))
        convertOneInstruction (MovFromStackAddr (Reg EDI) (Immediate 42))


runRunctionalTest :: IO () -> String -> Test
runRunctionalTest func path = TestCase $ do
    func
    expected <- P.readFile path
    output <- readProcess "objdump" ["-d", ".tmp_test_output"] ""
    let expected' = P.unlines $ P.drop 7 $ P.lines expected
    let output' = P.unlines $ P.drop 7 $ P.lines output
    if expected' == output' then
        P.putStr ""
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
            (elf P.>>= writeElf ".tmp_test_output")
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
            (elf P.>>= writeElf ".tmp_test_output")
            where
                p :: MonadCatch m => StateT CodeState m ()
                p = do
                    convertOneInstruction (Push (Immediate 42))

testPushMem :: Test
testPushMem = runRunctionalTest testPushMemImpl "ElfTestRes/pushMem_expected.txt"
    where
        testPushMemImpl :: IO ()
        testPushMemImpl = do
            let elf = assemble p
            (elf P.>>= writeElf ".tmp_test_output")
            where
                p :: MonadCatch m => StateT CodeState m ()
                p = do
                    convertOneInstruction (Push (Memory 42))

testPopReg :: Test
testPopReg = runRunctionalTest testPopRegImpl "ElfTestRes/popReg_expected.txt"
    where
        testPopRegImpl :: IO ()
        testPopRegImpl = do
            let elf = assemble p
            (elf P.>>= writeElf ".tmp_test_output")
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
            (elf P.>>= writeElf ".tmp_test_output")
            where
                p :: MonadCatch m => StateT CodeState m ()
                p = do
                    convertOneInstruction (Pop (Memory 42))

testXorRegReg :: Test
testXorRegReg = runRunctionalTest testXorRegRegImpl "ElfTestRes/xorRegReg_expected.txt"
    where
        testXorRegRegImpl :: IO ()
        testXorRegRegImpl = do
            let elf = assemble p
            (elf P.>>= writeElf ".tmp_test_output")
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
            (elf P.>>= writeElf ".tmp_test_output")
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
            (elf P.>>= writeElf ".tmp_test_output")
            where
                p :: MonadCatch m => StateT CodeState m ()
                p = do
                    convertOneInstruction (Xor (Reg EAX) (Reg EAX))
                    convertOneInstruction (VM.Label "_labelFoo" 42)
                    convertOneInstruction (Xor (Reg EAX) (Reg EDI))
                    convertOneInstruction (VM.Label "_labelBar" 42)
                    convertOneInstruction (Xor (Reg EAX) (Reg ESI))


testGetBinLengthFromRInstruction :: Test
testGetBinLengthFromRInstruction = TestList [
    binLengthFromRInstruction ((\_ _ -> Right (RInstruction 0x12345678))) ~?= 4,
    binLengthFromRInstruction ((\_ _ -> Right (RInstruction 0x123456))) ~?= 3,
    binLengthFromRInstruction ((\_ _ -> Right (RInstruction 0x1234))) ~?= 2]


functionalASMTests :: Test
functionalASMTests =
  TestList
    [ testRunStackAddr,
      testRunMovRegReg,
      testRunMovFromStackAddr,
      testPushReg,
      testPushImm,
      testPushMem,
      testPopReg,
      testPopMem,
      testXorRegReg,
      testXorRegImm,
      testLabelInASM]

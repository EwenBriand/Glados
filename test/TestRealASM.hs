{-# LANGUAGE DataKinds #-}
module TestRealASM
  ( testEncodeMov,
    testEncodeMovqRegImm,
    testmovOpcodeCombineRegReg,
    testEncodeMovRegReg,
    testEncodeMovMemImm,
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
  (elf P.>>= writeElf "ElfTestRes/testMovMemImm.o")

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

testEncodeMov :: Test
testEncodeMov =
  TestCase $ do
    testEncodeMovImpl
    assertBool "mov is correctly encoded" True
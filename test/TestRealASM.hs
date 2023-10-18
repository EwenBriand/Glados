module TestRealASM
  ( testEncodeMov,
    testEncodeMovqRegImm,
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

-- let state = StateT (\s -> (codeReversed s, s))

-- -- Extract the codeReversed field from the state
-- let reversedCode = evalStateT (gets codeReversed) state

testEncodeMovImpl :: IO ()
testEncodeMovImpl = do
  -- let codestate = dummyProgramSimpleMov
  --   let state = StateT (\s -> (codestate, s))
  --   let reversedCode = evalStateT (gets codeReversed) state
  --   print (reversedCode)
  let elf = assemble dummyProgramSimpleMov
  (elf P.>>= writeElf "ElfTestRes/testMovOpcode.o")

testEncodeMov :: Test
testEncodeMov =
  TestCase $ do
    testEncodeMovImpl
    assertBool "mov is correctly encoded" True

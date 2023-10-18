{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RealASM
  (
  )
where

import Control.Monad.Catch
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.ByteString.Builder
import Data.Elf
import Numeric (showHex)
import Data.Elf.PrettyPrint (printElf, printElf_, readFileLazy)
import Data.Elf.Constants
import Data.Elf.Headers
import Data.Proxy
import Data.Bits
import Data.ByteString.Lazy.Internal
import Data.Word

import VM

import Control.Monad.State
import ValidState

-------------------------------------------------------------------------------
-- Code State
-------------------------------------------------------------------------------

-- | Represents an offset in the code buffer. It is the index of the beginning of
-- the instruction in the buffer of binary encoded instructions.
newtype InstrAddress = InstrAddress { getInstrAddr :: Integer } deriving (Eq, Ord, Num, Enum, Real, Integral)

-- | Represents and instruction encoded in binary.
newtype InstrBin = Instr { getInstr :: Word32 } deriving (Eq, Ord, Num, Enum, Real, Integral)

-- | The type of functions that return the binary code of an instruction.
type InstrBinBuilder = InstrAddress -> InstrAddress -> InstrBin

-- | a label points to either a code address or a data address in the code state buffer.
data RealLabel = CodeRef InstrAddress | DataRef Integer deriving (Eq, Ord)
instance Show RealLabel where
    show (CodeRef (InstrAddress addr)) = "CodeRef " ++ show addr
    show (DataRef addr) = "DataRef " ++ show addr



-- | The state of the code generation. Updating it will queue the binary
-- instructions in the code buffer.
data CodeState = CodeState {
    offsetInPool :: InstrAddress,
    dataPoolReversed :: [Builder], -- binary of data elements in reverse order (fifo)
    codeBufferReversed :: [InstrBin], -- binary of instructions in reverse order (fifo)
    labelsReversed :: [(String, RealLabel)]
}


instance Show CodeState where
    show CodeState {..} = "CodeState { \n\toffsetInPool = " ++ show offsetInPool ++ ", \n\tdataPoolReversed = " ++ show dataPoolReversed ++ ", \n\tcodeBufferReversed = " ++ show codeBufferReversed ++ ", \n\tlabelsReversed = " ++ show labelsReversed ++ "}"

-- | The initial state of the code generation.
initCodeState :: CodeState
initCodeState = CodeState {
    offsetInPool = InstrAddress 0,
    dataPoolReversed = [],
    codeBufferReversed = [],
    labelsReversed = []
}

-- | Registers an instruction in the code buffer.




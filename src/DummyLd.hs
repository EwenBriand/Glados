{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DummyLd (
    dummyLd, 
    getMachineConfig, 
    MachineConfig (..)
) where

import Control.Monad.Catch
import Data.Bits

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Control.Exception.ChainedException
import Data.ByteString.Lazy as BSL

data MachineConfig a
    = MachineConfig
        { mcAddress :: WordXX a 
        , mcAlign   :: WordXX a 
        }

getMachineConfig :: (SingElfClassI a, MonadThrow m) => ElfMachine -> m (MachineConfig a)
getMachineConfig EM_AARCH64 = return $ MachineConfig 0x400000 0x10000
getMachineConfig EM_X86_64  = return $ MachineConfig 0x400000 0x1000
getMachineConfig _          = return $ MachineConfig 0x0 0x0

dummyLd' :: forall a m . (MonadThrow m, SingElfClassI a) => ElfListXX a -> Int -> m (ElfListXX a)
dummyLd' es entryOffset = do

    section' <- elfFindSectionByName es ".text"
    strtab <- elfFindSectionByName es ".strtab"
    symtab <- elfFindSectionByName es ".symtab"
    shstrtab <- elfFindSectionByName es ".shstrtab"

    txtSectionData <- case esData section' of
        ElfSectionData textData -> return textData

    header' <- (elfFindHeader es)
    MachineConfig { .. } <- getMachineConfig (ehMachine header')
    let header'' = header' {ehType = ET_EXEC}

    return $
        case header'' of
            ElfHeader { .. } ->
                ElfSegment
                    { epType       = PT_LOAD
                    , epFlags      = PF_X .|. PF_R
                    , epVirtAddr   = mcAddress
                    , epPhysAddr   = mcAddress
                    , epAddMemSize = 0
                    , epAlign      = mcAlign
                    , epData       =
                        ElfHeader
                            { ehType  = ET_EXEC
                            , ehEntry = mcAddress + headerSize (fromSingElfClass $ singElfClass @a) + (toEnum (fromIntegral entryOffset) :: WordXX a)
                            , ..
                            }
                        ~: ElfRawData
                            { edData = txtSectionData
                            }
                        ~: ElfListNull
                    }
                    ~: section'
                    ~: shstrtab
                    ~: ElfSegmentTable
                    ~: ElfSectionTable
                    ~: ElfListNull


dummyLd :: MonadThrow m => Int -> Elf -> m Elf
dummyLd entryOffset (Elf c l) = Elf c <$> withSingElfClassI c (dummyLd' l entryOffset)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DummyLd (dummyLd) where

import Control.Monad.Catch
import Data.Bits

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Control.Exception.ChainedException
import Data.ByteString.Lazy as BSL

data MachineConfig a
    = MachineConfig
        { mcAddress :: WordXX a -- ^ Virtual address of the executable segment
        , mcAlign   :: WordXX a -- ^ Required alignment of the executable segment
                                --   in physical memory (depends on max page size)
        }

getMachineConfig :: (SingElfClassI a, MonadThrow m) => ElfMachine -> m (MachineConfig a)
getMachineConfig EM_AARCH64 = return $ MachineConfig 0x400000 0x10000
getMachineConfig EM_X86_64  = return $ MachineConfig 0x400000 0x1000
getMachineConfig _          = $chainedError "could not find machine config for this arch"

dummyLd' :: forall a m . (MonadThrow m, SingElfClassI a) => ElfListXX a -> Int -> m (ElfListXX a)
dummyLd' es entryOffset = do

    section' <- elfFindSectionByName es ".text"
    strtab <- elfFindSectionByName es ".strtab"
    symtab <- elfFindSectionByName es ".symtab"
    shstrtab <- elfFindSectionByName es ".shstrtab"

    txtSectionData <- case esData section' of
        ElfSectionData textData -> return textData
        _ -> $chainedError "could not find correct \".text\" section"

    -- FIXME: it's better to match constructor here, but there is a bug that prevents to conclude that
    -- the match is irrefutable:
    -- https://stackoverflow.com/questions/72803815/phantom-type-makes-pattern-matching-irrefutable-but-that-seemingly-does-not-wor
    -- https://gitlab.haskell.org/ghc/ghc/-/issues/15681#note_165436
    -- But if I use lazy pattern match, then some other bug comes up that prevents type inference
    -- on GHC 9.0.2
    header' <- (elfFindHeader es)
    MachineConfig { .. } <- getMachineConfig (ehMachine header')
    let header'' = header' {ehType = ET_EXEC}
   
            -- ElfHeader
            --     { ehData = ELFDATA2LSB, -- little endian
            --     -- {  ehData = ELFDATA2MSB, -- big endian
            --         ehOSABI = ELFOSABI_SYSV,
            --         ehABIVersion = 0,
            --         ehType = ET_EXEC,
            --         ehMachine = EM_X86_64,
            --         ehEntry = 0,
            --         ehFlags = 0
            --     }

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
                    }                    -- ~: ElfSegment
                    --     { epType       = PT_LOAD
                    --     , epFlags      = PF_R .|. PF_W  -- .data section flags (read and write)
                    --     , epVirtAddr   = mcAddress  -- .data virtual address
                    --     , epPhysAddr   = mcAddress  -- .data physical address
                    --     , epAddMemSize = 0  -- .data section size
                    --     , epAlign      = mcAlign  -- .data section alignment
                    --     , epData       =
                    --         ElfHeader
                    --             { ehType  = ET_EXEC
                    --             , ehEntry = mcAddress + headerSize (fromSingElfClass $ singElfClass @a) + (toEnum (fromIntegral entryOffset) :: WordXX a)
                    --             , ..
                    --             }
                    --         ~: ElfRawData
                    --             { edData = dataSectionData  -- Define the data for .data section
                    --             }
                    --         ~: ElfListNull
                    --     }
                    ~: section'
                    ~: shstrtab
                    ~: ElfSegmentTable
                    ~: ElfSectionTable
                    ~: ElfListNull


-- | @dummyLd@ places the content of ".text" section of the input ELF
-- into the loadable segment of the resulting ELF.
-- This could work if there are no relocations or references to external symbols.

-- concatenateElfLists :: Elf -> Elf -> Elf
-- concatenateElfLists (Elf cls1 list1) (Elf cls2 list2) =
--     (Elf cls1 <$> withSingElfClassI cls1 (concatElfLists list1 list2))

-- concatElfLists :: SingElfClassI a => ElfListXX a -> ElfListXX a -> SingElfClass a -> ElfListXX a
-- concatElfLists ElfListNull list _ = list
-- concatElfLists (ElfListCons elf list1) list2 c =
--     ElfListCons elf (concatElfLists list1 list2 c)

dummyLd :: MonadThrow m => Int -> Elf -> m Elf
dummyLd entryOffset (Elf c l) = Elf c <$> withSingElfClassI c (dummyLd' l entryOffset)
    -- do
    -- l' <- withSingElfClassI c dummyLd' l entryOffset
    -- return $ Elf c (concatElfLists l l' c)

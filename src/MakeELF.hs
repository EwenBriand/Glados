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

module MakeELF
  (
    debugLoadAndShowElf,
  )
where

import Control.Monad.Catch
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Elf
import Numeric (showHex)
import Data.Elf.PrettyPrint (printElf, printElf_, readFileLazy)
import Data.Elf.Constants
import Data.Elf.Headers
import Data.Proxy
import Data.Bits
import Data.ByteString.Lazy.Internal

import VM

loadBin :: FilePath -> IO BL.ByteString
loadBin path = do
  bin <- B.readFile path
  return $ BL.fromStrict bin

debugPrintJustElf :: Elf -> IO ()
debugPrintJustElf elf = print "oui le type est ok"

easyPrintElf :: Elf -> IO ()
easyPrintElf elf = do
  doc <- printElf_ False elf
  print doc

debugLoadAndShowElf :: FilePath -> IO ()
debugLoadAndShowElf path = do
    bs <- fromStrict <$> BS.readFile path
    elf <- parseElf bs
    easyPrintElf loadDefaultElf
    -- easyPrintElf elf
    -- doc <- printElf_ False elf
    -- print doc


-- | Create an ElfLabel from a its name and a bool that is true if the symbol is to be global.
-- It is assumed that:
--  - the symbol is in the .text section which is the first section of the elf
--  - the symbol is a function
makeElfLabel :: String -> Bool -> ElfSymbolXX 'ELFCLASS64
makeElfLabel name isGlobal = let
        steName = name
        steBind = if isGlobal then STB_Global else STB_Local
        steType = STT_NoType
        steShNdx = 1
        steValue = 0
        steSize = 0
    in ElfSymbolXX {..}

serializeSymbol :: ElfSymbolXX 'ELFCLASS64 -> B.ByteString
serializeSymbol ElfSymbolXX {..} = B.concat
    [
        serializeWord32 (fromIntegral $ fromEnum steName)
        , serializeWord8 (fromIntegral $ fromEnum steBind)
        , serializeWord8 (fromIntegral $ fromEnum steType)
        , serializeWord16 (fromIntegral steShNdx)
        , serializeWord64 steValue
        , serializeWord64 steSize
    ]

-- | Load a default ELF file for an x64 machine, without any symbols or code.
loadDefaultElf :: Elf
loadDefaultElf = Elf SELFCLASS64 $
        ElfHeader
            { ehData       = ELFDATA2LSB
            , ehOSABI      = ELFOSABI_SYSV
            , ehABIVersion = 0
            , ehType       = ET_REL
            , ehMachine    = EM_AARCH64
            , ehEntry      = 0
            , ehFlags      = 0
            }
        ~: ElfSection {
            esName        = ".text"
            , esType      = SHT_PROGBITS
            , esFlags     = SHF_EXECINSTR .|. SHF_ALLOC
            , esAddr      = 0
            , esAddrAlign = 8
            , esEntSize   = 0
            , esN         = 0
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionData Data.ByteString.Lazy.Internal.Empty -- update this with the program's code
            }
        ~: ElfSection {
            esName         = ".symtab"
            , esType       = SHT_SYMTAB
            , esFlags      = 0
            , esAddr       = 0
            , esAddrAlign  = 8
            , esEntSize    = 24 -- not 18 like in hello.o? investigate
            , esN          = 0
            , esLink       = 0
            , esInfo       = 0
            , esData       = serializeSymbol $ makeElfLabel "_start" True
            }
        ~: ElfSectionTable
        ~: ElfListNull



updateElfFromContext :: Context -> Elf -> Elf
updateElfFromContext context elf = elf -- todo change the elf
contextToElf :: Context -> Elf
contextToElf context =
    updateElfFromContext context loadDefaultElf

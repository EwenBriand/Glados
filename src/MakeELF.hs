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

module MakeELF
  ( debugLoadAndShowElf,
    compileAndPrint,
  )
where

import AsmAArch64
import Control.Monad.Catch
import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.ByteString as B
import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.ByteString.Lazy.Internal
import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Data.Elf.PrettyPrint (printElf, printElf_, readFileLazy)
import qualified Data.List as Map
import Data.Proxy
import Data.Word
import GHC.Word
import Numeric (showHex)
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
  let elf' = loadDefaultElf
  easyPrintElf elf'

-- easyPrintElf elf
-- doc <- printElf_ False elf
-- print doc

-- | Create an ElfLabel from a its name and a bool that is true if the symbol is to be global.
-- It is assumed that:
--  - the symbol is in the .text section which is the first section of the elf
--  - the symbol is a function
makeElfLabel :: String -> Bool -> ElfSymbolXX 'ELFCLASS64
makeElfLabel name isGlobal =
  let steName = name
      steBind = if isGlobal then STB_Global else STB_Local
      steType = STT_NoType
      steShNdx = ElfSectionIndex 2
      steValue = 0
      steSize = 0
   in ElfSymbolXX {..}

serializeWord8 :: GHC.Word.Word8 -> Data.ByteString.Lazy.Internal.ByteString
serializeWord8 = BL.singleton

serializeWord16 :: GHC.Word.Word16 -> Data.ByteString.Lazy.Internal.ByteString
serializeWord16 = runPut . putWord16be

serializeWord32 :: GHC.Word.Word32 -> Data.ByteString.Lazy.Internal.ByteString
serializeWord32 = runPut . putWord32be

serializeWord64 :: GHC.Word.Word64 -> Data.ByteString.Lazy.Internal.ByteString
serializeWord64 = runPut . putWord64be

serializeSymbol :: ElfSymbolXX 'ELFCLASS64 -> Data.ByteString.Lazy.Internal.ByteString
serializeSymbol ElfSymbolXX {..} =
  BL.concat
    [ serializeWord32 (fromIntegral $ Prelude.length steName),
      serializeWord8 (fromIntegral $ fromEnum steBind),
      serializeWord8 (fromIntegral $ fromEnum steType),
      serializeWord16 (fromIntegral steShNdx),
      serializeWord64 steValue,
      serializeWord64 steSize
    ]

-- | Load a default ELF file for an x64 machine, without any symbols or code.
loadDefaultElf :: Elf
loadDefaultElf =
  Elf SELFCLASS64 $
    ElfHeader
      { ehData = ELFDATA2LSB,
        ehOSABI = ELFOSABI_SYSV,
        ehABIVersion = 0,
        ehType = ET_REL,
        ehMachine = EM_AARCH64,
        ehEntry = 0,
        ehFlags = 0
      }
      ~: ElfSectionTable
      ~: ElfSection
        { esName = ".text",
          esType = SHT_PROGBITS,
          esFlags = SHF_EXECINSTR .|. SHF_ALLOC,
          esAddr = 0,
          esAddrAlign = 8,
          esEntSize = 0,
          esN = ElfSectionIndex 1,
          esLink = 0,
          esInfo = 0,
          esData = ElfSectionData Data.ByteString.Lazy.Internal.Empty -- update this with the program's code
        }
      ~: ElfSection
        { esName = ".symtab",
          esType = 3,
          esFlags = 0,
          esAddr = 0,
          esAddrAlign = 8,
          esEntSize = 0,
          esN = ElfSectionIndex 2,
          esLink = 0,
          esInfo = 0,
          esData = ElfSectionData (serializeSymbol $ makeElfLabel "_start" True) -- update this with the program's code
        }
      ~: ElfSection
        { esName = ".symtab",
          esType = SHT_SYMTAB,
          esFlags = 0,
          esAddr = 0,
          esAddrAlign = 16,
          esEntSize = symbolTableEntrySize ELFCLASS64, -- not 18 like in hello.o? investigate
          esN = 4,
          esLink = 3,
          esInfo = 1,
          esData = ElfSectionData (serializeSymbol $ makeElfLabel "_start" True)
        }
      ~: ElfListNull

updateElfFromContext :: Context -> Elf -> Elf
updateElfFromContext context elf = elf -- todo change the elf

contextToElf :: Context -> Elf
contextToElf context =
  updateElfFromContext context loadDefaultElf

compileAndPrint :: Context -> IO ()
compileAndPrint context = do
  let elf = contextToElf context
  easyPrintElf elf

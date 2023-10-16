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
-- import Data.Singletons
-- import Data.Singletons.TypeLits
import Numeric (showHex)
import Data.Elf.PrettyPrint (printElf, printElf_, readFileLazy)

loadBin :: FilePath -> IO BL.ByteString
loadBin path = do
  bin <- B.readFile path
  return $ BL.fromStrict bin


-- loadElf :: MonadCatch m => BL.ByteString -> m Elf
-- loadElf = parseElf

-- printElf :: ElfList c -> IO ()
-- printElf elfList = do
--   case elfList of
--     ElfList elfXXList -> do
--       -- Assuming the ELF header is the first element of the list
--       case elfXXList of
--         (elf : _) -> do
--           Prelude.putStrLn "ELF Header:"
--           Prelude.putStrLn $ "  Magic: " ++ show (ehOSABI elf)
--           Prelude.putStrLn $ "  Type: " ++ show (ehType elf)
--           Prelude.putStrLn $ "  Machine: " ++ show (ehMachine elf)
--           Prelude.putStrLn $ "  Version: " ++ show (ehABIVersion elf)
--           Prelude.putStrLn $ "  Entry point address: 0x" ++ showHex (ehEntry elf) ""
--           Prelude.putStrLn $ "  Flags: " ++ showHex (ehFlags elf) ""
--         _ -> Prelude.putStrLn "No ELF header found"

debugPrintJustElf :: Elf -> IO ()
debugPrintJustElf elf = print "oui le type est ok"

debugLoadAndShowElf :: FilePath -> IO ()
debugLoadAndShowElf path = do
    bs <- fromStrict <$> BS.readFile path
    elf <- parseElf bs
    doc <- printElf_ False elf
    print doc


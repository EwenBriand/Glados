{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import REPL
import VM
import ValidState
import System.Console.CmdArgs
import EvaluateAST (strToHASM)
import System.IO
import System.Exit
import REPL
import qualified Data.Map as Map
import Control.Monad (mapM_)
import MakeELF
import AsmAArch64
import Includes

data Options = Options {
    binary :: String,
    srcRaw :: String,
    outputFile :: String,
    disassemble :: Bool,
    execute :: Bool,
    compileObject :: String,
    fileExecutable :: String,
    ast :: Bool
} deriving (Show, Data, Typeable)

options :: Options
options = Options {
    binary = def &= args &= typ "SRCFILE",
    srcRaw = def &= help "The text source file to execute or compile" &= typFile,
    outputFile = def &= help "The output file to write the compiled binary to" &= typFile,
    disassemble = def &= help "Prints the disassembled binary",
    execute = def &= help "Execute the binary loaded / created",
    compileObject = def &= help "The path to the object file to produce" &= typFile,
    fileExecutable = def &= help "The path to the executable file to produce" &= typFile,
    ast = def &= help "Prints the AST of the source file"
} &= summary "Very SAD GladOs Compiler & Interpreter V1.0" -- SAD: Simple And Dumb, Splendid And Direct, Service After Death, or maybe just sad ;)

getContextOnOps :: Options -> IO (ValidState Context)
getContextOnOps ops = do
    if binary ops /= "" then
        loadContext (binary ops)
    else if srcRaw ops /= "" then do
            src <- readFile (srcRaw ops)
            src' <- resolveIncludes src
            Prelude.return (detectLabels (strToHASM (Valid newContext) src'))
        else do
            putStrLn "Awaiting input: (Ctrl-d to end input)\n"
            hFlush stdout
            c <- strToHASM (Valid newContext) <$> readContents
            Prelude.return (detectLabels c)

printBlocks :: Context -> BlockMap -> IO ()
printBlocks c bm = if Map.null (blockMap bm) then
    putStr ""
    else do
        mapM_ (\(k, v) -> do
            putStrLn ("\n" ++ k ++ ":")
            showDisassembly (blockContext v)) (Map.toList (blockMap bm))

printInstructions :: [Instruction ] -> IO ()
printInstructions [] = putStr ""
printInstructions (i:is) = do
    print i
    printInstructions is

showDisassembly :: ValidState Context -> IO ()
showDisassembly (Invalid s) = putStrLn ("Context invalidated: " ++ s) >> exitWith (ExitFailure 84)
showDisassembly (Valid c) = do
    printInstructions (instructions c)
    printBlocks c (blocks c)

showAST :: ValidState Context -> IO ()
showAST (Invalid s) = putStrLn ("Context invalidated: " ++ s) >> exitWith (ExitFailure 84)
showAST (Valid c) = do
    print (cAST c)
    mapM_ (\(k, v) -> do
        putStrLn ("\n" ++ k ++ ":")
        showAST (blockContext v)) (Map.toList (blockMap (blocks c)))

execOnOps :: IO (ValidState Context) -> Options -> IO ()
execOnOps ctx ops = do
    c <- ctx
    mapM_ (\(cond, exec) -> do
        if cond then
            exec c
        else
            putStr "") handlers
    where
        handlers = [
            (fileExecutable ops /= "", \ct -> compileInFileWrapper ct (fileExecutable ops) True),
            (compileObject ops /= "", \ct -> compileInFileWrapper ct (compileObject ops) False),
            (disassemble ops, showDisassembly),
            (ast ops, showAST),
            (execute ops, execImpl),
            (outputFile ops /= "", \ct -> saveContext ct (outputFile ops))]


switchOnOptions :: Options -> IO ()
switchOnOptions ops = execOnOps (getContextOnOps ops) ops


main :: IO ()
main = do
    opts <- cmdArgs options
    switchOnOptions opts

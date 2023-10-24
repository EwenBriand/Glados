{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import REPL
import VM
-- import Lexer
-- import EvaluateAST
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

data Options = Options {
    binary :: String,
    srcRaw :: String,
    outputFile :: String,
    disassemble :: Bool,
    execute :: Bool,
    compileObject :: String,
    fileExecutable :: String
} deriving (Show, Data, Typeable)

options :: Options
options = Options {
    binary = def &= args &= typ "SRCFILE",
    srcRaw = def &= help "The text source file to execute or compile" &= typFile,
    outputFile = def &= help "The output file to write the compiled binary to" &= typFile,
    disassemble = def &= help "Prints the disassembled binary",
    execute = def &= help "Execute the binary loaded / created",
    compileObject = def &= help "The path to the object file to produce" &= typFile,
    fileExecutable = def &= help "The path to the executable file to produce" &= typFile
} &= summary "Very SAD GladOs Compiler & Interpreter V1.0" -- SAD: Simple And Dumb, Splendid And Direct, Service After Death, or maybe just sad ;)

getContextOnOps :: Options -> IO (ValidState Context)
getContextOnOps ops = do
    if binary ops /= "" then
        loadContext (binary ops)
    else if srcRaw ops /= "" then do
            src <- readFile (srcRaw ops)
            Prelude.return (detectLabels (strToHASM (Valid newContext) src))
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

addAutoExit :: Context -> Context
addAutoExit c = c { instructions = instructions c ++ [
    Xor (Reg EBX) (Reg EBX),
    Mov (Reg EBX) (Reg EAX),
    Mov (Reg EAX) (Immediate 1),
    Interrupt]}

execOnOps :: IO (ValidState Context) -> Options -> IO ()
execOnOps ctx ops =
    if fileExecutable ops /= "" then
        do
        c <- ctx
        case c of
            Invalid s -> putStrLn ("Context invalidated: " ++ s) >> exitWith (ExitFailure 84)
            Valid ct -> compileInFile (addAutoExit ct) (fileExecutable ops) True
    else if compileObject ops /= "" then
        do
        c <- ctx
        case c of
            Invalid s -> putStrLn ("Context invalidated: " ++ s) >> exitWith (ExitFailure 84)
            Valid ct -> compileInFile (ct) (compileObject ops) False
    else do
        c <- ctx
        if disassemble ops then
            showDisassembly c
        else
            putStr ""
        if execute ops then
            execImpl c
        else putStr ""
        if outputFile ops /= "" then
            saveContext c (outputFile ops)
        else putStr ""


switchOnOptions :: Options -> IO ()
switchOnOptions ops = execOnOps (getContextOnOps ops) ops


main :: IO ()
main = do
    opts <- cmdArgs options
    switchOnOptions opts

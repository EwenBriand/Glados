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
import REPL

data Options = Options {
    binary :: String,
    srcRaw :: String,
    outputFile :: String,
    disassemble :: Bool,
    execute :: Bool
} deriving (Show, Data, Typeable)

options :: Options
options = Options {
    binary = def &= args &= typ "SRCFILE",
    srcRaw = def &= help "The raw source file to execute or compile" &= typFile,
    outputFile = def &= help "The output file to write" &= typFile,
    disassemble = def &= help "Prints the disassembled binary",
    execute = def &= help "Execute the binary loaded / created"
} &= summary "Very SAD GladOs Compiler & Interpreter" -- SAD: Simple And Dumb, Splendid And Direct, or maybe just sad ;)

getContextOnOps :: Options -> IO (ValidState Context)
getContextOnOps ops = do
    if binary ops /= "" then
        loadContext (binary ops)
    else if srcRaw ops /= "" then do
            src <- readFile (srcRaw ops)
            Prelude.return (strToHASM (Valid newContext) src)
        else do
            putStrLn "Awaiting input: (Ctrl-d to end input)\n"
            hFlush stdout
            strToHASM (Valid newContext) <$> readContents

execOnOps :: IO (ValidState Context) -> Options -> IO ()
execOnOps ctx ops = do
    c <- ctx
    if disassemble ops then
        print c
    else
        print ""
    if execute ops then
        execImpl c
    else print ""
    if outputFile ops /= "" then
        saveContext c (outputFile ops)
    else print ""


switchOnOptions :: Options -> IO ()
switchOnOptions ops = execOnOps (getContextOnOps ops) ops


main :: IO ()
main = do
    opts <- cmdArgs options
    switchOnOptions opts

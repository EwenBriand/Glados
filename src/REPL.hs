
-- | This module contains the REPL for the language.
-- The REPL is the interactive shell that allows the user to enter commands
-- and see the result of those commands. The code entered is immediately
-- compiled and executed.


module REPL (
    runREPL
) where

import System.IO (hFlush, stdout)
import MyLexer

-- Runs an interactive console that allows the user to enter commands,
-- and redirects these commands to the lexer in order to build and evaluate the
-- AST.
runREPL :: IO()
runREPL = do
    putStr "_> "
    hFlush stdout
    input <- getLine
    if input == "exit"
        then return ()
        else do
            putStrLn $ show $ strToAST input
            runREPL


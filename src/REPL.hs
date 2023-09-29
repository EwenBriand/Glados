
-- | This module contains the REPL for the language.
-- The REPL is the interactive shell that allows the user to enter commands
-- and see the result of those commands. The code entered is immediately
-- compiled and executed.


module REPL (
    runREPL
) where

import System.IO (hFlush, stdout)
import Lexer
import EvaluateAST (strToHASM)
import VM
import Instructions
import qualified Data.Maybe

restartREPL :: Maybe Context -> IO()
restartREPL Nothing = runREPL Nothing
-- restartREPL (Just c) = runREPL (Just c {instructions = [], registers = newRegisters, instructionPointer = 0})
restartREPL (Just c) = runREPL (Just c { instructions = drop 2 (instructions c)})
-- restartREPL (Just c) = runREPL (Just c {instructions = [], instructionPointer=0})

-- Runs an interactive console that allows the user to enter commands,
-- and redirects these commands to the lexer in order to build and evaluate the
-- AST.
runREPL :: Maybe Context -> IO()
runREPL Nothing = runREPL (Just newContext)
runREPL (Just c) = do
    putStr "_> "
    hFlush stdout
    input <- getLine
    if input == "exit"
        then return ()
        else do
            case strToHASM (Just c) input of
                Nothing -> putStrLn "Error no HASM"
                Just ctx -> do
                    print ctx
                    let c' = execInstructions (Just ctx)
                    print c'
                    case getTrueValueFromParam c' (Reg EAX) of
                        Nothing -> putStrLn "Error EAX is null"
                        Just v -> print v
                    restartREPL c'
                    -- runREPL c' {instructions = []}


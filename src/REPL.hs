
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
import System.IO

restartREPL :: Maybe Context -> IO()
restartREPL Nothing = runREPL Nothing
restartREPL (Just c) = runREPL (Just c { instructions = drop 2 (instructions c)})
-- restartREPL (Just c) = runREPL (Just c { instructions = drop 2 (instructions c)})

-- | Reads the contents from the command line until there is no more text to read.
-- @return: the contents of the command line as a single string
-- readContents :: IO String
readContents = do
    isEof <- isEOF
    if isEof
        then
            return ""
        else do
            c <- getChar
            rest <- readContents
            return (c:rest)

parseLabels :: Maybe Context -> [Instruction] -> Int -> Maybe Context
parseLabels Nothing _ _ = Nothing
parseLabels (Just c) [] _ = Just c
parseLabels (Just c) (i:is) idx = case i of
    Label name _ -> parseLabels (labelSet (Just c) name idx) is (idx + 1)
    _ -> parseLabels (Just c) is (idx + 1)


-- detects the labels in the instructions and adds them to the context
detectLabels :: Maybe Context -> Maybe Context
detectLabels Nothing = Nothing
detectLabels (Just c) = parseLabels (Just c) (instructions c) 0

-- Runs an interactive console that allows the user to enter commands,
-- and redirects these commands to the lexer in order to build and evaluate the
-- AST.
runREPL :: Maybe Context -> IO()
runREPL Nothing = runREPL (Just newContext)
runREPL (Just c) = do
    putStr "_> "
    hFlush stdout
    input <- readContents
    print input
    if input == "exit"
        then return ()
        else do
            case strToHASM (Just c) input of
                Nothing -> putStrLn "Error no HASM"
                Just ctx -> do
                    let c' = execInstructions (detectLabels (Just ctx))
                    case getTrueValueFromParam c' (Reg EAX) of
                        Nothing -> putStrLn "Error EAX is null"
                        Just v -> print v
                    print c'
                    -- restartREPL c'
                    -- runREPL c' {instructions = []}


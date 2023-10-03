
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
import System.IO
import ValidState

restartREPL :: ValidState Context -> IO()
restartREPL (Invalid s) = runREPL (Invalid s)
restartREPL (Valid c) = runREPL (Valid c { instructions = drop 2 (instructions c)})
-- restartREPL (Valid c) = runREPL (Valid c { instructions = drop 2 (instructions c)})

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

parseLabels :: ValidState Context -> [Instruction] -> Int -> ValidState Context
parseLabels (Invalid s) _ _ = Invalid s
parseLabels (Valid c) [] _ = Valid c
parseLabels (Valid c) (i:is) idx = case i of
    Label name _ -> parseLabels (labelSet (Valid c) name idx) is (idx + 1)
    _ -> parseLabels (Valid c) is (idx + 1)


-- detects the labels in the instructions and adds them to the context
detectLabels :: ValidState Context -> ValidState Context
detectLabels (Invalid s) = Invalid s
detectLabels (Valid c) = parseLabels (Valid c) (instructions c) 0

-- Runs an interactive console that allows the user to enter commands,
-- and redirects these commands to the lexer in order to build and evaluate the
-- AST.
runREPL :: ValidState Context -> IO()
runREPL (Invalid _) = runREPL (Valid newContext)
runREPL (Valid c) = do
    putStr "_> "
    hFlush stdout
    input <- readContents
    print input
    if input == "exit"
        then return ()
        else do
            case strToHASM (Valid c) input of
                Invalid s -> putStrLn s
                Valid ctx -> do
                    let c' = execInstructions (detectLabels (Valid ctx))
                    case getTrueValueFromParam c' (Reg EAX) of
                        Invalid s -> putStrLn s
                        Valid v -> print v
                    print c'
                    -- restartREPL c'
                    -- runREPL c' {instructions = []}


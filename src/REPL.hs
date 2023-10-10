-- | This module contains the REPL for the language.
-- The REPL is the interactive shell that allows the user to enter commands
-- and see the result of those commands. The code entered is immediately
-- compiled and executed.
module REPL
  ( runREPL,
  readContents,
  logicLoop,
  execImpl
  )
where

-- import System.IO
import EvaluateAST (strToHASM)
import Instructions
import Lexer
import System.IO
import VM
import ValidState
import System.Exit

restartREPL :: ValidState Context -> IO ()
restartREPL (Invalid s) = runREPL (Invalid s)
restartREPL (Valid c) = runREPL (Valid c {instructions = drop 2 (instructions c)})

-- restartREPL (Valid c) = runREPL (Valid c { instructions = drop 2 (instructions c)})

-- | Reads the contents from the command line until there is no more text to read.
-- @return: the contents of the command line as a single string
readContents :: IO String
readContents = do
  isEof <- isEOF
  if isEof
    then Prelude.return ""
    else do
      c <- getChar
      rest <- readContents
      Prelude.return (c : rest)

showInstructionRange :: Context -> Int -> Int -> IO ()
showInstructionRange c start end = do
  if start == end
    then Prelude.return ()
    else do
      print (instructions c !! start)
      showInstructionRange c (start + 1) end

-- shows error string, followed by the instructions until the instruction pointer
showTrace :: ValidState Context -> String -> [ASTNode] -> IO ()
showTrace (Invalid s) _ a = putStrLn ("Context invalidated: " ++ s ++ "\nAST: \n" ++ show a)
showTrace (Valid c) s a = do
  putStrLn (s ++ "\nAST: \n" ++ show a)
  showInstructionRange c 0 (instructionPointer c)

-- Runs an interactive console that allows the user to enter commands,
-- and redirects these commands to the lexer in order to build and evaluate the
-- AST.
-- ! DEPRECATED, USE EXECIMPL INSTEAD
runREPL :: ValidState Context -> IO ()
runREPL (Invalid _) = runREPL (Valid newContext)
runREPL (Valid c) = do
    putStr "_> "
    hFlush stdout
    input <- readContents
    if input == "exit"
        then Prelude.return ()
        else do
            case strToHASM (Valid c) input of
                Invalid s -> putStrLn s >> exitWith (ExitFailure 84)
                Valid ctx -> logicLoop (execInstructionsIO (detectLabels (Valid ctx), putStr ""))

execImpl :: ValidState Context -> IO ()
execImpl (Invalid s) = putStrLn s >> exitWith (ExitFailure 84)
execImpl (Valid c) = logicLoop (execInstructionsIO (Valid c, putStr ""))


logicLoop :: (ValidState Context, IO()) -> IO()
logicLoop (Invalid s, io) = io >> putStrLn s >> exitWith (ExitFailure 84)
logicLoop (Valid c, io) = if instructionPointer c >= length (instructions c)
    then io >> print (fromValidState (-1) (getTrueValueFromParam (Valid c) (Reg EAX)))
    else io >> logicLoop (execInstructionsIO (detectLabels (Valid c), putStr ""))
    -- io >> logicLoop (execInstructionsIO (detectLabels (Valid c)))

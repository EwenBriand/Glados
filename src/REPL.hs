-- | This module contains the REPL for the language.
-- The REPL is the interactive shell that allows the user to enter commands
-- and see the result of those commands. The code entered is immediately
-- compiled and executed.
module REPL
  ( runREPL,
  )
where

-- import System.IO
import EvaluateAST (strToHASM)
import Instructions
import Lexer
import System.IO
import VM
import ValidState

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
runREPL :: ValidState Context -> IO ()
runREPL (Invalid _) = runREPL (Valid newContext)
runREPL (Valid c) = do
  putStr "_> "
  hFlush stdout
  input <- readContents
--   print input
  if input == "exit"
    then Prelude.return ()
    else do
      case strToHASM (Valid c) input of
        Invalid s -> putStrLn s
        Valid ctx -> do
          let ast = cAST ctx
          let c' = execInstructions (detectLabels (Valid ctx))
          case getTrueValueFromParam c' (Reg EAX) of
            Invalid s -> showTrace c' s ast
            Valid v -> print v

-- restartREPL c'
-- runREPL c' {instructions = []}

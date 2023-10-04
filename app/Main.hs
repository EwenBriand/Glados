module Main (main) where

import REPL
import VM
-- import Lexer
-- import EvaluateAST
import ValidState

main :: IO ()
main = runREPL (Valid newContext)


module Main (main) where

import REPL
import VM
import Lexer
import EvaluateAST

main :: IO ()
main = runREPL Nothing


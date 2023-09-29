module Main (main) where

import REPL
import MySyscall(
    SyscallCode(..),
    execSyscall)
import VM
import Lexer
import EvaluateAST

main :: IO ()
main = do
    let c = astNodeArrayToHASM (Just (newContext)) (ASTNodeArray [ASTNodeInteger 1, ASTNodeInteger 2])
    case c of
        Nothing -> putStrLn "Error!"
        Just ctx -> print (instructions ctx)



module Main (main) where

import REPL
import MySyscall(
    SyscallCode(..),
    execSyscall)
import VM

main :: IO ()
main = case ioValue of
    Nothing -> putStrLn "Nothing"
    Just io -> io
    where
        (_, ioValue) = execSyscall ctx   SCEasyPrint
        ctx = regSet (Just newContext) EAX 123


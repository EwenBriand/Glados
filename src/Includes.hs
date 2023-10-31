{-# LANGUAGE PatternGuards #-}
module Includes (
    resolveIncludes,
    beginsWith,
    removeChar,
    getFiles
) where

import Data.Maybe
import Data.List
import System.Environment



beginsWith :: String -> String -> Bool
beginsWith [] _ = True
beginsWith _ [] = False
beginsWith (x:xs) (y:ys) = if x == y then beginsWith xs ys else False

removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar c (x:xs) = if c == x then removeChar c xs else x : removeChar c xs

getFiles :: String -> IO [String]
getFiles filelist = do
    let (filename, rest) = break (== ',') filelist
    let filename' = if beginsWith "\"" filename then init (tail filename) else filename
    let filename'' = removeChar '\n' filename'
    let filename''' = removeChar ' ' filename''
    cwd <- getEnv "PWD"
    putStrLn ("Loading file " ++ cwd ++ "/" ++ filename''' ++ "...")
    file <- readFile (cwd ++ "/" ++ filename''')
    file' <- resolveIncludes file
    files <- if rest == [] then Prelude.return [] else getFiles (tail rest)
    Prelude.return (file' : files)

resolveIncludes :: String -> IO String
resolveIncludes src = if "#include" `isInfixOf` src then do
    let (before, after) = break (== '{') src
    let (filelist, rest) = break (== '}') (tail after)
    files <- getFiles filelist
    Prelude.return (Prelude.concat files ++ tail rest)
    else Prelude.return src

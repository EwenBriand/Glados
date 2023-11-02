{-# LANGUAGE PatternGuards #-}
module Includes (
    resolveIncludes,
    resolveMacros,
    beginsWith,
    removeChar,
    getFiles,
    breakEndArgs,
    replaceAllArgsOfMacro,
    MacroList(..),
    Macro(..),
) where

import Data.Maybe
import Data.List
import System.Environment
import Debug.Trace
import Data.Char (isSpace)
---------------------------------------------
-- Utils
---------------------------------------------

beginsWith :: String -> String -> Bool
beginsWith [] _ = True
beginsWith _ [] = False
beginsWith (x:xs) (y:ys) = if x == y then beginsWith xs ys else False

removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar c (x:xs) = if c == x then removeChar c xs else x : removeChar c xs

---------------------------------------------
-- Includes
---------------------------------------------
-- #include {
--     file1.gld,
--     file2.gld,
--     file3.gld
-- }
---------------------------------------------

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


---------------------------------------------
-- Macros
---------------------------------------------
-- #macro {
--     ADD(x, y) x + y
--     NEG(x) -x
-- }
---------------------------------------------

data Macro = Macro {
    name :: String,
    args2 :: [String],
    def2 :: String
}

instance Show Macro where
    show (Macro name args2 def2) = "Macro:\n\t(name) " ++ name ++ "\n\t(args2) " ++ show args2 ++ "\n\t(def) " ++ def2 ++ "\n"

data MacroList = MacroList {
    macros :: [Macro]
} deriving (Show)

newMacroList :: MacroList
newMacroList = MacroList {macros = []}

addMacro :: MacroList -> Macro -> MacroList
addMacro (MacroList macros) macro = MacroList {macros = macro : macros}

breakMacro :: String -> (String, String, String, String)
breakMacro src = do
    let (name, rest) = break (== '(') src
    let (args2, rest') = break (== ')') (tail rest)
    let (def2, rest'') = break (== '\n') (tail rest')
    let rest''' = if null rest'' then [] else tail rest''
    trace ("Current name: " ++ name) $ (name, args2, def2, rest''')


removeWhitespace :: String -> String
removeWhitespace = filter (not . isSpace)

fetchAllMacros :: String -> MacroList
fetchAllMacros "" = newMacroList
fetchAllMacros src = do
    let (name, args2, def2, rest') = breakMacro src
    let macro = Macro {name = removeWhitespace name, args2 = words args2, def2 = removeWhitespace def2}
    trace ("Current macro: " ++ show macro) $ addMacro (fetchAllMacros rest') macro

replaceSubstring :: String -> String -> String -> String
replaceSubstring str old new = go str
  where
    go [] = []
    go str'@(c:cs)
      | old `isPrefixOf` str' = new ++ go (drop (length old) str')
      | otherwise = c : go cs

replaceAllArgsOfMacro :: String -> [String] -> [String] -> String
replaceAllArgsOfMacro def2 [] _ = def2
replaceAllArgsOfMacro def2 _ [] = def2
replaceAllArgsOfMacro def2 (arg:args2) (arg':args2') = do
    let def2' = replaceSubstring def2 arg arg'
    replaceAllArgsOfMacro def2' args2 args2'

replaceMacro :: Macro -> String -> [String] -> String
replaceMacro macro src list_arg = do
    let def2'' = replaceAllArgsOfMacro (def2 macro) (args2 macro) list_arg
    replaceSubstring src ((name macro) ++ "(" ++ findsArgs macro src ++ ")") def2''

removePrefix :: String -> String -> String
removePrefix "" _ = ""
removePrefix str prefix = if prefix `isPrefixOf` str
    then drop (length prefix) str
    else removePrefix (tail str) prefix

breakEndArgs :: String -> Int -> (String, String)
breakEndArgs "" nbPar = ("", "")
breakEndArgs (x:xs) nbPar 
    | x == ')' && nbPar == 1 = ("", xs)
    | x == ')' = let (before, after) = breakEndArgs xs (nbPar - 1) in (x : before, after)
    | x == '(' = let (before, after) = breakEndArgs xs (nbPar + 1) in (x : before, after)
    | otherwise = let (before, after) = breakEndArgs xs nbPar in (x : before, after)

findsArgs :: Macro -> String -> String
findsArgs macro src =
    let src' = removePrefix src (name macro)
    in if null src' then ""
       else let (_, after') = break (== '(') src'
                (args2, _) = breakEndArgs (tail after') 1
            in trace ("find args " ++ show args2) $ args2

splitArgs :: String -> [String]
splitArgs "" = []
splitArgs src = do
    let (arg, rest) = break (== ',') src
    let arg' = if beginsWith "\"" arg then init (tail arg) else arg
    let arg'' = removeChar '\n' arg'
    let arg''' = removeChar ' ' arg''
    let rest' = if null rest then [] else tail rest
    arg''' : splitArgs rest'

replaceAllMacroCall :: Macro -> String -> String
replaceAllMacroCall macro src = do
    let args2 = splitArgs (findsArgs macro src)
    if length args2 > 0 then do
            let src' = replaceMacro macro src args2
            trace ("replace " ++ show macro ++ " in " ++ src') $ replaceAllMacroCall macro src'
        else src

replaceMacroForEach :: MacroList -> String -> String
replaceMacroForEach (MacroList []) src = src
replaceMacroForEach (MacroList (m:ms)) src = do
    let src' = replaceAllMacroCall m src
    trace ("replace for each " ++ show m ++ " in " ++ src') $ replaceMacroForEach (MacroList ms) src'

replaceMacroForEachCall :: String -> String -> String
replaceMacroForEachCall macrolist src = do
    let macroList = fetchAllMacros macrolist
    let src' = replaceMacroForEach macroList src
    trace ("current macrolist" ++ show macrolist ++ " and src is " ++ show src') $ src'

resolveMacros :: String -> IO String
resolveMacros src = if "#macro" `isInfixOf` src then do
    let (before, after) = break (== '{') src
    let (macrolist, rest) = break (== '}') (tail after)
    let src' = replaceMacroForEachCall (init macrolist) (tail rest)
    Prelude.return src'
    else Prelude.return src

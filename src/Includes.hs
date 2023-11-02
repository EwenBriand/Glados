{-# LANGUAGE PatternGuards #-}
module Includes (
    resolveIncludes,
    resolveMacros,
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
    args :: [String],
    def :: String
}

instance Show Macro where
    show (Macro name args def) = "Macro:\n\t(name) " ++ name ++ "\n\t(args) " ++ show args ++ "\n\t(def) " ++ def ++ "\n"

data MacroList = MacroList {
    macros :: [Macro]
} deriving (Show)

newMacroList :: MacroList
newMacroList = MacroList {macros = []}

addMacro :: MacroList -> Macro -> MacroList
addMacro (MacroList macros) macro = MacroList {macros = macro : macros}

clearMacroList :: MacroList -> MacroList
clearMacroList (MacroList macros) = MacroList {macros = []}

getMacro :: MacroList -> String -> Maybe Macro
getMacro (MacroList macros) name' = if null macros then Nothing else do
    let macro = head macros
    if name' == name macro then Just macro else getMacro (MacroList (tail macros)) name'

-- breakMacro :: String -> (String, String, String, String)
-- breakMacro src = do
--     let (name, rest) = break (== '(') src
--     let (args, rest') = break (== ')') (tail rest)
--     let (def, rest'') = break (== '\n') (tail rest')
--     let rest''' = if null rest'' then [] else tail rest''
--     (name, args, def, rest''')

breakMacro :: String -> (String, String, String, String)
breakMacro src = do
    let (name, rest) = break (== '(') src
    let (args, rest') = break (== ')') (tail rest)
    let (def, rest'') = break (== '\n') (tail rest')
    let rest''' = if null rest'' then [] else tail rest''
    trace ("Current name: " ++ name) $ (name, args, def, rest''')


-- fetchAllMacros :: String -> MacroList
-- fetchAllMacros src = if "#macro" `isInfixOf` src then do
--     let (before, after) = break (== '{') src
--     let (macrolist, rest) = break (== '}') (tail after)
--     let (name, args, def, rest') = breakMacro macrolist
--     let macro = Macro {name = name, args = words args, def = def}
--     addMacro (fetchAllMacros rest') macro
--     else newMacroList

removeWhitespace :: String -> String
removeWhitespace = filter (not . isSpace)

fetchAllMacros :: String -> MacroList
fetchAllMacros "" = newMacroList
fetchAllMacros src = do
    let (name, args, def, rest') = breakMacro src
    let macro = Macro {name = removeWhitespace name, args = words args, def = removeWhitespace def}
    trace ("Current macro: " ++ show macro) $ addMacro (fetchAllMacros rest') macro

replace :: String -> String -> String -> String
replace [] _ _ = []
replace src old new = if old `isInfixOf` src then do
    let (before, after) = break (== head old) src
    let (_, after') = break (== last old) after
    before ++ new ++ replace after' old new
    else src

replaceSubstring :: String -> String -> String -> String
replaceSubstring str old new = go str
  where
    go [] = []
    go str'@(c:cs)
      | old `isPrefixOf` str' = new ++ go (drop (length old) str')
      | otherwise = c : go cs

replaceAllArgsOfMacro :: String -> [String] -> [String] -> String
replaceAllArgsOfMacro def [] _ = def
replaceAllArgsOfMacro def _ [] = def
replaceAllArgsOfMacro def (arg:args) (arg':args') = do
    let def' = replaceSubstring def arg arg'
    replaceAllArgsOfMacro def' args args'

replaceMacro :: Macro -> String -> [String] -> String
replaceMacro macro src list_arg = do
    let def'' = replaceAllArgsOfMacro (def macro) (args macro) list_arg
    replaceSubstring src ((name macro) ++ "(" ++ findsArgs macro src ++ ")") def''

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
                (args, _) = breakEndArgs (tail after') 1
            in trace ("find args " ++ show args) $ args

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
    let args = splitArgs (findsArgs macro src)
    if length args > 0 then do
            let src' = replaceMacro macro src args
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

removeMacroDirective :: String -> String
removeMacroDirective src = if "#macro" `isInfixOf` src then do
    let (before, after) = break (== '{') src
    let (_, rest) = break (== '}') (tail after)
    if null rest then before else before ++ tail rest
    else src

resolveMacros :: String -> IO String
resolveMacros src = if "#macro" `isInfixOf` src then do
    let (before, after) = break (== '{') src
    let (macrolist, rest) = break (== '}') (tail after)
    let src' = replaceMacroForEachCall (init macrolist) (tail rest)
    let src'' = removeMacroDirective src'
    Prelude.return src''
    else Prelude.return src

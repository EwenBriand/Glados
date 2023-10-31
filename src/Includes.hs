{-# LANGUAGE PatternGuards #-}
module Includes (
    resolveIncludes,
    resolveMacros,
) where

import Data.Maybe
import Data.List
import System.Environment

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

fetchAllMacros :: String -> MacroList
fetchAllMacros src = if "#macro" `isInfixOf` src then do
    let (before, after) = break (== '{') src
    let (macrolist, rest) = break (== '}') (tail after)
    let (name, args, def, rest') = breakMacro macrolist
    let macro = Macro {name = name, args = words args, def = def}
    addMacro (fetchAllMacros rest') macro
    else newMacroList

removeMacro :: MacroList -> String -> MacroList
removeMacro (MacroList macros) name' = if null macros then newMacroList else do
    let macro = head macros
    if name' == name macro then MacroList (tail macros) else removeMacro (MacroList (tail macros)) name'

breakMacro :: String -> (String, String, String, String)
breakMacro src = do
    let (name, rest) = break (== '(') src
    let (args, rest') = break (== ')') (tail rest)
    let (def, rest'') = break (== '\n') (tail rest')
    let rest''' = if null rest'' then [] else tail rest''
    (name, args, def, rest''')

replace :: String -> String -> String -> String
replace [] _ _ = []
replace src old new = if old `isInfixOf` src then do
    let (before, after) = break (== head old) src
    let (_, after') = break (== last old) after
    before ++ new ++ replace after' old new
    else src

replaceMacro :: Maybe Macro -> String -> String -> String -> String
replaceMacro Nothing _ _ _ = []
replaceMacro (Just macro) args def rest = do
    let (arg, rest') = break (== ',') args
    let arg' = if beginsWith "\"" arg then init (tail arg) else arg
    let arg'' = removeChar '\n' arg'
    let arg''' = removeChar ' ' arg''
    let def' = replace def arg''' arg
    let rest'' = if null rest' then [] else tail rest'
    if null rest'' then def' else replaceMacro (Just macro) rest'' def' rest''

replaceMacroForEachCall :: String -> String -> String
replaceMacroForEachCall macrolist src = do
    -- add every macro to the list
    let macroList = fetchAllMacros macrolist
    -- parse the source file and replace every macro call with its definition
    let (name, args, def, rest) = breakMacro src
    let macro = getMacro macroList name
    let src' = replaceMacro macro args def rest
    if null rest then src' else replaceMacroForEachCall macrolist src'

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
    let src' = replaceMacroForEachCall macrolist rest
    src'' <- resolveMacros src'
    Prelude.return (before ++ src'')
    else Prelude.return src

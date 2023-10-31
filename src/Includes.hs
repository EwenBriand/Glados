{-# LANGUAGE PatternGuards #-}
module Includes (
    resolveIncludes,
    resolveMacros,
) where

import Data.Maybe
import Data.List
import System.Environment
import Debug.Trace

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


fetchAllMacros :: String -> MacroList
fetchAllMacros src = do
    let (name, args, def, rest') = breakMacro src
    let macro = Macro {name = name, args = words args, def = def}
    trace ("Current macro: " ++ show macro) $ addMacro (fetchAllMacros rest') macro

replace :: String -> String -> String -> String
replace [] _ _ = []
replace src old new = if old `isInfixOf` src then do
    let (before, after) = break (== head old) src
    let (_, after') = break (== last old) after
    before ++ new ++ replace after' old new
    else src

replaceMacroByItsDef :: Maybe Macro -> String -> String -> String -> String
replaceMacroByItsDef Nothing _ _ _ = []
replaceMacroByItsDef (Just macro) args def rest = do
    let args' = words args
    let def' = replace def (head args') (head args')
    let def'' = replace def' (head (tail args')) (head (tail args'))
    let def''' = replace def'' (head (tail (tail args'))) (head (tail (tail args')))
    replace rest (name macro) def'''

detectMacroCall :: String -> String -> Maybe Macro
detectMacroCall src name' = if name' `isInfixOf` src then do
    let (before, after) = break (== '(') src
    let (_, rest) = break (== ')') (tail after)
    let (name, args, def, rest') = breakMacro before
    if name == name' then Just Macro {name = name, args = words args, def = def} else detectMacroCall rest name'
    else Nothing

replaceMacroForEachCall :: String -> String -> String
replaceMacroForEachCall macrolist src = do
    let macroList = fetchAllMacros macrolist
    let macro = detectMacroCall src (name (head (macros macroList)))
    if isNothing macro then src else do
        let macro' = fromJust macro
        let src' = replaceMacroByItsDef (getMacro macroList (name macro')) (unwords (args macro')) (def macro') src
        trace ("Current src: " ++ src') $ replaceMacroForEachCall macrolist src'

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
    let src'' = removeMacroDirective src'
    Prelude.return src''
    else Prelude.return src

module TestIncludes
(
    testNoIncludes,
    testIncludes,
    testVariousIncludes,
    functionalIncTests
) where

import Lexer
import Includes
import REPL
import Test.HUnit
import Options

testNoIncludes :: Test
testNoIncludes = TestCase (do
    src <- readFile "tests/ptr.gld"
    src' <- resolveIncludes src
    assertEqual "source didn't change" src src')

testIncludes :: Test
testIncludes = TestCase (do
    src <- readFile "tests/includes/code.gld"
    src' <- resolveIncludes src
    assertBool "source changed" (src /= src'))

acquirefiles :: [String] -> IO [String]
acquirefiles = Prelude.mapM readFile

assertEqIOStrings :: IO [String] -> IO [String] -> IO Bool
assertEqIOStrings ioStrings1 ioStrings2 = do
    strings1 <- ioStrings1
    strings2 <- ioStrings2
    return (strings1 == strings2)

testVariousIncludes :: Test
testVariousIncludes = TestList [
    "begins with true" ~: beginsWith [] "coucou la" ~?= True,
    "begins with false" ~: beginsWith "coucou la" [] ~?= False,
    "begins with x==y" ~: beginsWith "haha" "haha" ~?= True,
    "filename starts with quotes" ~: assertEqIOStrings (getFiles "\"tests/ptr.gld\"") (acquirefiles ["tests/ptr.gld"]),
    "several files" ~: assertEqIOStrings (getFiles "\"tests/ptr.gld\",\"tests/ptr.gld\"") (acquirefiles ["tests/ptr.gld", "tests/ptr.gld"]),
    "several includes" ~: (do
    src <- readFile "tests/includes/code2.gld"
    src' <- resolveIncludes src
    assertBool "source changed" (src /= src'))
  ]

testResolveMacro :: Test
testResolveMacro = TestCase (do
    src <- readFile "tests/macros/code3.gld"
    src' <- resolveMacros src
    src'' <- readFile "tests/macros/code3.txt"
    assertBool "source changed" (src'' == src'))

testNotResolveMacro :: Test
testNotResolveMacro = TestCase (do
    src <- readFile "tests/includes/code2.gld"
    src' <- resolveMacros src
    assertBool "source changed" (src == src'))

testBreakEndArgs :: Test
testBreakEndArgs = TestList
  [ "breakEndArgs should handle an empty string" ~: do
        assertEqual "breakEndArgs \"\" 0 should be (\"\", \"\")" (breakEndArgs "" 0) ("", "")
  , "breakEndArgs should handle a string with no parentheses" ~: do
        let input = "hello world!"
        assertEqual "breakEndArgs input 0 should be (input, \"\")" (breakEndArgs input 0) (input, "")
  , "breakEndArgs should handle a string with one pair of parentheses" ~: do
        let input = "hello (world)!"
        assertEqual "breakEndArgs input 0 should be (\"hello (world)\", \"!\")" (breakEndArgs input 0) ("hello (world", "!")
  , "breakEndArgs should handle a string with nested parentheses" ~: do
        let input = "hello (world (of (Haskell)))!"
        assertEqual "breakEndArgs input 0 should be (\"hello (world (of (Haskell)))\", \"!\")" (breakEndArgs input 0) ("hello (world (of (Haskell))", "!")
  , "breakEndArgs should handle a string with unmatched parentheses" ~: do
        let input = "hello (world!"
        assertEqual "breakEndArgs input 0 should be (\"hello (world!\", \"\")" (breakEndArgs input 0) ("hello (world!", "")
  ]

testReplaceAllArgsOfMacro :: Test
testReplaceAllArgsOfMacro = TestList
  [ "replaceAllArgsOfMacro should handle an empty list of arguments" ~: do
        let input = "hello world!"
        assertEqual "replaceAllArgsOfMacro input [] [] should be input" (replaceAllArgsOfMacro input [] []) input
  , "replaceAllArgsOfMacro should handle different length argument lists" ~: do
        let input = "hello Haskell!"
        let args = ["world", "Haskell"]
        let args' = ["Haskell"]
        assertEqual "replaceAllArgsOfMacro input args args' should be input" (replaceAllArgsOfMacro input args args') input
  , "replaceAllArgsOfMacro should handle missing arguments" ~: do
        let input = "hello world!"
        let args = ["Haskell"]
        let args' = ["Python"]
        assertEqual "replaceAllArgsOfMacro input args args' should be input" (replaceAllArgsOfMacro input args args') input
  , "replaceAllArgsOfMacro should handle empty argument strings" ~: do
        let input = "hello world!"
        let args = ["world"]
        let args' = [""]
        assertEqual "replaceAllArgsOfMacro input args args' should be \"hello \"" (replaceAllArgsOfMacro input args args') "hello !"
  , "replaceAllArgsOfMacro should handle longer argument strings" ~: do
        let input = "hello world!"
        let args = ["world"]
        let args' = ["Haskell"]
        assertEqual "replaceAllArgsOfMacro input args args' should be \"hello Haskell!\"" (replaceAllArgsOfMacro input args args') "hello Haskell!"
  , "replaceAllArgsOfMacro should handle shorter argument strings" ~: do
        let input = "hello world!"
        let args = ["world"]
        let args' = ["!"]
        assertEqual "replaceAllArgsOfMacro input args args' should be \"hello!\"" (replaceAllArgsOfMacro input args args') "hello !!"
  ]

testMacroListShow :: Test
testMacroListShow = TestList
  [ "show should handle an empty list of macros" ~: do
        let input = MacroList { macros = [] }
        assertEqual "show input should be \"MacroList {macros = []}\"" (show input) "MacroList {macros = []}"
  , "show should handle a list of macros" ~: do
        let input = MacroList { macros = [Macro { name = "macro1", args2 = ["arg1"], def2 = "def1" }, Macro { name = "macro2", args2 = ["arg2"], def2 = "def2" }] }
        assertEqual "show input should be \"MacroList {macros = [Macro {name = \\\"macro1\\\", args = [\\\"arg1\\\"], def = \\\"def1\\\"}, Macro {name = \\\"macro2\\\", args = [\\\"arg2\\\"], def = \\\"def2\\\"}]}\"" (show input) "MacroList {macros = [Macro:\n\t(name) macro1\n\t(args2) [\"arg1\"]\n\t(def) def1\n,Macro:\n\t(name) macro2\n\t(args2) [\"arg2\"]\n\t(def) def2\n]}"
  , "show should handle a list of macros2" ~: do
        let input = MacroList { macros = [Macro { name = "macro1", args2 = ["arg1"], def2 = "def1" }, Macro { name = "macro2", args2 = ["arg2"], def2 = "def2" }] }
        assertEqual "show child" (show (macros input)) "[Macro:\n\t(name) macro1\n\t(args2) [\"arg1\"]\n\t(def) def1\n,Macro:\n\t(name) macro2\n\t(args2) [\"arg2\"]\n\t(def) def2\n]"
  ]

functionalIncTests :: Test
functionalIncTests =
  TestList [
    testResolveMacro,
    testNotResolveMacro,
    testBreakEndArgs,
    testReplaceAllArgsOfMacro,
    testMacroListShow
  ]

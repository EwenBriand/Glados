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

testResolvMacro :: Test
testResolvMacro = TestCase (do
    src <- readFile "tests/macros/code3.gld"
    src' <- resolveMacros src
    src'' <- readFile "tests/macros/code3.txt"
    assertBool "source changed" (src'' == src'))

functionalIncTests :: Test
functionalIncTests =
  TestList [
    testResolvMacro
  ]

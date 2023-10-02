module TestLexer (
    testInstructionList,
    testASTNodeFields,
    testShowASTNode,
    testTokOrExprToNode,
    testTryToMatch,
    testBuildASTIterate,
    testBuildAST,
    testTryBuildInstructionList,
    testStrToAST
) where

import Test.HUnit
import Lexer
import Tokenizer


testInstructionList :: Test
testInstructionList = TestList [
    "valid: " ~: strToAST "(+ 1 1)\n(+ 2 2)\n" ~?= ASTNodeInstructionSequence [ASTNodeSum [ASTNodeInteger 1, ASTNodeInteger 1], ASTNodeSum [ASTNodeInteger 2, ASTNodeInteger 2]]]

testASTNodeFields :: Test
testASTNodeFields = TestList
  [ "Test astnerrToken field" ~: do
      let node = ASTNodeError (TokenInfo TokError "error message")
      let expectedToken = TokenInfo TokError "error message"
      assertEqual "astnerrToken should match" expectedToken (astnerrToken node)
  , "Test astniValue field" ~: do
      let node = ASTNodeInteger 42
      let expectedValue = 42
      assertEqual "astniValue should match" expectedValue (astniValue node)
  , "Test astnsName field" ~: do
      let node = ASTNodeSymbol "symbol"
      let expectedName = "symbol"
      assertEqual "astnsName should match" expectedName (astnsName node)
  , "Test astndName field" ~: do
      let node = ASTNodeDefine (ASTNodeSymbol "symbol") (ASTNodeInteger 42)
      let expectedName = ASTNodeSymbol "symbol"
      assertEqual "astndName should match" expectedName (astndName node)
  , "Test astndChildren field" ~: do
      let node = ASTNodeDefine (ASTNodeSymbol "symbol") (ASTNodeInteger 42)
      let expectedChildren = ASTNodeInteger 42
      assertEqual "astndChildren should match" expectedChildren (astndChildren node)
  , "Test astnsChildren field" ~: do
      let node = ASTNodeSum [ASTNodeInteger 1, ASTNodeInteger 2]
      let expectedChildren = [ASTNodeInteger 1, ASTNodeInteger 2]
      assertEqual "astnsChildren should match" expectedChildren (astnsChildren node)
  , "Test astndChildrenDebug field" ~: do
      let node = ASTNodeDebug [T (TokenInfo TokInteger "1"), T (TokenInfo TokInteger "2")]
      let expectedChildren = [T (TokenInfo TokInteger "1"), T (TokenInfo TokInteger "2")]
      assertEqual "astndChildrenDebug should match" expectedChildren (astndChildrenDebug node)
  , "Test astnplChildren field" ~: do
      let node = ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]
      let expectedChildren = [ASTNodeInteger 1, ASTNodeInteger 2]
      assertEqual "astnplChildren should match" expectedChildren (astnplChildren node)
  , "Test astnaChildren field" ~: do
      let node = ASTNodeArray [ASTNodeInteger 1, ASTNodeInteger 2]
      let expectedChildren = [ASTNodeInteger 1, ASTNodeInteger 2]
      assertEqual "astnaChildren should match" expectedChildren (astnaChildren node)
  , "Test astnisChildren field" ~: do
      let node = ASTNodeInstructionSequence [ASTNodeInteger 1, ASTNodeInteger 2]
      let expectedChildren = [ASTNodeInteger 1, ASTNodeInteger 2]
      assertEqual "astnisChildren should match" expectedChildren (astnisChildren node)
  ]

testShowASTNode :: Test
testShowASTNode = test
  [ "Test Show instance for ASTNode" ~:
    let
      exampleNode = ASTNodeInteger 42
      expectedString = "ASTNodeInteger {astniValue = 42}"
    in do
      assertEqual "Show instance should match" expectedString (show exampleNode)
  ]

testTokOrExprToNode :: Test
testTokOrExprToNode =
  TestList
    [ "node error" ~: tokOrExprToASTNode [] ~?= ASTNodeError (TokenInfo TokError ""),
      "node integer" ~: tokOrExprToASTNode [T (TokenInfo TokInteger "123")] ~?= ASTNodeInteger 123,
      "node sum" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSum [ASTNodeInteger 1, ASTNodeInteger 2],
      "node sub" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorMinus "-"), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSub [ASTNodeInteger 1, ASTNodeInteger 2],
      "node mul" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorMul "*"), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeMul [ASTNodeInteger 1, ASTNodeInteger 2],
      "node div" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorDiv "/"), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeDiv [ASTNodeInteger 1, ASTNodeInteger 2],
      "node mod" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorMod "%"), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeMod [ASTNodeInteger 1, ASTNodeInteger 2]
    ]

testTryToMatch :: Test
testTryToMatch =
  TestList
    [ "no match" ~: tryToMatch [] (T (TokenInfo TokError "")) [] ~?= (A (ASTNodeError (TokenInfo TokError "")), []),
      "node match integer 0" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokInteger "123")] ~?= (A (ASTNodeInteger {astniValue = 123}), []),
      "node match integer 1" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "567")] ~?= (A (ASTNodeInteger {astniValue = 123}), [T (TokenInfo TokInteger "567")]),
      "node match integer 2" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "567"), T (TokenInfo TokInteger "000")] ~?= (A (ASTNodeInteger {astniValue = 123}), [T (TokenInfo TokInteger "567"), T (TokenInfo TokInteger "000")]),
      "node match simple sum" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeParamList [ASTNodeInteger 123, ASTNodeInteger 678]), T (TokenInfo TokCloseParen ")")] ~?= (A (ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678]), [])
    ]

-- try to parse the tokens that make the following expression: (+ 1 (+ 2 3))
-- the test should return a sum node that contains an integer node and another sum node
testBuildASTIterate :: Test
testBuildASTIterate =
  TestList
    [ "build ast sum" ~: buildASTIterate [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeParamList [ASTNodeInteger 123, ASTNodeInteger 678]), T (TokenInfo TokCloseParen ")")] ~?= [A (ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678])],
      "build ast integer 1" ~: buildASTIterate [T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "567")] ~?= [A (ASTNodeInteger 123), A (ASTNodeInteger 567)],
      "build middle index" ~: buildASTIterate [T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "678"), T (TokenInfo TokCloseParen ")")] ~?= [A (ASTNodeInteger 123), A (ASTNodeInteger 678), T (TokenInfo TokCloseParen ")")],
      "incomplete iteration" ~: buildASTIterate [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "678"), T (TokenInfo TokCloseParen ")")] ~?= [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeInteger 123), A (ASTNodeInteger 678), T (TokenInfo TokCloseParen ")")]
    ]

testBuildAST :: Test
testBuildAST =
  TestList
    [ -- (+ 123 678)
      "build ast sum" ~: buildAST [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "678"), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678],
      -- (+ 123 (+ 678 000))
      "build ast nested sum 1" ~: buildAST [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "678"), T (TokenInfo TokInteger "000"), T (TokenInfo TokCloseParen ")"), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSum [ASTNodeInteger 123, ASTNodeSum [ASTNodeInteger 678, ASTNodeInteger 0]],
      -- (+ (+ 123 678) 000)
      "build ast nested sum 2" ~: buildAST [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "678"), T (TokenInfo TokCloseParen ")"), T (TokenInfo TokInteger "000"), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSum [ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678], ASTNodeInteger 0],
      -- (+ (+ 123) 2)
      "build ast error invalid expr" ~: buildAST [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokCloseParen ")"), T (TokenInfo TokInteger "2"), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeError (TokenInfo TokError "cannot resolve input")
    ]

testTryBuildInstructionList :: Test
testTryBuildInstructionList =
  TestList
    [ "Test tryBuildInstructionList with empty input" ~:
        tryBuildInstructionList []
        ~?= ASTNodeError (TokenInfo TokError "empty"),
      "Test tryBuildInstructionList with single ParamList" ~:
        tryBuildInstructionList [A (ASTNodeParamList [ASTNodeError (TokenInfo TokError "param")])]
        ~?= ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "param")],
      "Test tryBuildInstructionList with single InstructionSequence" ~:
        tryBuildInstructionList [A (ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "inst")])]
        ~?= ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "inst")],
      "Test tryBuildInstructionList with append InstructionSequence" ~:
        tryBuildInstructionList [A (ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "inst")]), A (ASTNodeError (TokenInfo TokError "newinst"))]
        ~?= ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "inst"), ASTNodeError (TokenInfo TokError "newinst")],
      "Test tryBuildInstructionList with two Instructions" ~:
        tryBuildInstructionList [A (ASTNodeError (TokenInfo TokError "inst1")), A (ASTNodeError (TokenInfo TokError "inst2"))]
        ~?= ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "inst1"), ASTNodeError (TokenInfo TokError "inst2")]
    ]


testStrToAST :: Test
testStrToAST = TestList [
    -- (+ 123 678)
    "build str to ast sum" ~: strToAST "(+ 123 678)" ~?= ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678],
    -- (+ (+ 123) 2)
    "build str to ast invalid" ~: strToAST "(+ (+ 123) 2)" ~?= ASTNodeError (TokenInfo TokError "cannot resolve input"),
    -- (define foo 123)
    "declare var foo with value 123" ~: strToAST "(define foo 123)" ~?= ASTNodeDefine (ASTNodeSymbol "foo") (ASTNodeInteger 123)]

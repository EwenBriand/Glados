module TestLexer
  ( testInstructionList,
    testASTNodeFields,
    testShowASTNode,
    testTokOrExprToNode,
    testTryToMatch,
    testBuildASTIterate,
    testBuildAST,
    testTryBuildInstructionList,
    testStrToAST,
    testShowVarType,
    testShowTokorNode,
    moreTestsTokOrExprToNode,
    testShowInstanceASTNode,
    testIsSymbolAndParamArray,
    moreTestFunctions,
    testLexerConstructor
  )
where

import Lexer
import Test.HUnit
import Tokenizer
import ValidState

testInstructionList :: Test
testInstructionList =
  TestList
    [ "valid: " ~: strToAST "(+ 1 1)\n(+ 2 2)\n" ~?= ASTNodeInstructionSequence [ASTNodeSum [ASTNodeInteger 1, ASTNodeInteger 1], ASTNodeSum [ASTNodeInteger 2, ASTNodeInteger 2]]
    ]

testASTNodeFields :: Test
testASTNodeFields =
  TestList
    [ "Test astnerrToken field" ~: do
        let node = ASTNodeError (TokenInfo TokError "error message")
        let expectedToken = TokenInfo TokError "error message"
        assertEqual "astnerrToken should match" expectedToken (astnerrToken node),
      "Test astniValue field" ~: do
        let node = ASTNodeInteger 42
        let expectedValue = 42
        assertEqual "astniValue should match" expectedValue (astniValue node),
      "Test astnsName field" ~: do
        let node = ASTNodeSymbol "symbol"
        let expectedName = "symbol"
        assertEqual "astnsName should match" expectedName (astnsName node),
      "Test astndName field" ~: do
        let node = ASTNodeSet (ASTNodeSymbol "symbol") (ASTNodeInteger 42)
        let expectedName = ASTNodeSymbol "symbol"
        assertEqual "astndName should match" expectedName (astndName node),
      "Test astndChildren field" ~: do
        let node = ASTNodeSet (ASTNodeSymbol "symbol") (ASTNodeInteger 42)
        let expectedChildren = ASTNodeInteger 42
        assertEqual "astndChildren should match" expectedChildren (astndChildren node),
      "Test astnsChildren field" ~: do
        let node = ASTNodeSum [ASTNodeInteger 1, ASTNodeInteger 2]
        let expectedChildren = [ASTNodeInteger 1, ASTNodeInteger 2]
        assertEqual "astnsChildren should match" expectedChildren (astnsChildren node),
      "Test astndChildrenDebug field" ~: do
        let node = ASTNodeDebug [T (TokenInfo TokInteger "1"), T (TokenInfo TokInteger "2")]
        let expectedChildren = [T (TokenInfo TokInteger "1"), T (TokenInfo TokInteger "2")]
        assertEqual "astndChildrenDebug should match" expectedChildren (astndChildrenDebug node),
      "Test astnplChildren field" ~: do
        let node = ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]
        let expectedChildren = [ASTNodeInteger 1, ASTNodeInteger 2]
        assertEqual "astnplChildren should match" expectedChildren (astnplChildren node),
      "Test astnaChildren field" ~: do
        let node = ASTNodeArray [ASTNodeInteger 1, ASTNodeInteger 2]
        let expectedChildren = [ASTNodeInteger 1, ASTNodeInteger 2]
        assertEqual "astnaChildren should match" expectedChildren (astnaChildren node),
      "Test astnisChildren field" ~: do
        let node = ASTNodeInstructionSequence [ASTNodeInteger 1, ASTNodeInteger 2]
        let expectedChildren = [ASTNodeInteger 1, ASTNodeInteger 2]
        assertEqual "astnisChildren should match" expectedChildren (astnisChildren node),
      "Test astnbValue field" ~: do
        let node = ASTNodeBoolean True
        let expectedValue = True
        assertEqual "astnbValue should match" expectedValue (astnbValue node)
    ]

testShowASTNode :: Test
testShowASTNode =
  test
    [ "Test Show instance for ASTNode"
        ~: let exampleNode = ASTNodeInteger 42
               expectedString = "(int: 42)"
            in do
                 assertEqual "Show instance should match" expectedString (show exampleNode)
    ]

testShowVarType :: Test
testShowVarType =
  test
    [ "Test Show instance for VarType"
        ~: let exampleVarType = GInt
               expectedString = "GInt"
            in do
                 assertEqual "Show instance should match" expectedString (show exampleVarType)
    ]

testShowTokorNode :: Test
testShowTokorNode =
  test
    [ "Test Show instance for TokorNode"
        ~: let exampleTokorNode = T (TokenInfo TokInteger "42")
               expectedString = "42"
            in do
                 assertEqual "Show instance should match" expectedString (show exampleTokorNode)
    ]

testTokOrExprToNode :: Test
testTokOrExprToNode =
  TestList
    [ "node error" ~: tokOrExprToASTNode [] ~?= ASTNodeError (TokenInfo TokError ""),
      "node integer" ~: tokOrExprToASTNode [T (TokenInfo TokInteger "123")] ~?= ASTNodeInteger 123,
      "node symbol" ~: tokOrExprToASTNode [T (TokenInfo TokSymbol "abc")] ~?= ASTNodeSymbol "abc",
      -- "node define" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokKeywordDefine "define"), A (ASTNodeSymbol "foo"), A (ASTNodeInteger 123), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeDefine (ASTNodeSymbol "foo") (ASTNodeArray [(ASTNodeInteger 123)]),
      "node param 1" ~: tokOrExprToASTNode [A (ASTNodeParamList [ASTNodeInteger 1]), A (ASTNodeInteger 2)] ~?= ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2],
      "node sum" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSum [ASTNodeInteger 1, ASTNodeInteger 2],
      "node sub" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorMinus "-"), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSub [ASTNodeInteger 1, ASTNodeInteger 2],
      "node mul" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorMul "*"), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeMul [ASTNodeInteger 1, ASTNodeInteger 2],
      "node div" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorDiv "/"), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeDiv [ASTNodeInteger 1, ASTNodeInteger 2],
      "node mod" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorMod "%"), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeMod [ASTNodeInteger 1, ASTNodeInteger 2],
      "node true" ~: tokOrExprToASTNode [T (TokenInfo TokenBool "true")] ~?= ASTNodeBoolean True,
      "node error" ~: tokOrExprToASTNode [T (TokenInfo TokError "#?!&")] ~?= ASTNodeError (TokenInfo TokError "[#?!&]")
    ]

testTryToMatch :: Test
testTryToMatch =
  TestList
    [ "no match" ~: tryToMatch [] (T (TokenInfo TokError "")) [] ~?= (A (ASTNodeError (TokenInfo TokError "")), []),
      "node match integer 0" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokInteger "123")] ~?= (A (ASTNodeInteger 123), []),
      "node match integer 1" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "567")] ~?= (A (ASTNodeInteger 123), [T (TokenInfo TokInteger "567")]),
      "node match integer 2" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "567"), T (TokenInfo TokInteger "000")] ~?= (A (ASTNodeInteger 123), [T (TokenInfo TokInteger "567"), T (TokenInfo TokInteger "000")]),
      "node match simple sum" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeParamList [ASTNodeInteger 123, ASTNodeInteger 678]), T (TokenInfo TokCloseParen ")")] ~?= (A (ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678]), [])
    ]

-- try to parse the tokens that make the following expression: (+ 1 (+ 2 3))
-- the test should Prelude.return a sum node that contains an integer node and another sum node
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
      "build ast error invalid expr" ~: buildAST [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokCloseParen ")"), T (TokenInfo TokInteger "2"), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeError (TokenInfo TokError "[(,+,(,+,(int: 123),),(int: 2),)]"),
      -- empty
      "build ast empty" ~: buildAST [] ~?= ASTNodeError (TokenInfo TokError "empty")
    ]

testTryBuildInstructionList :: Test
testTryBuildInstructionList =
  TestList
    [ "Test tryBuildInstructionList with empty input"
        ~: tryBuildInstructionList []
        ~?= ASTNodeError (TokenInfo TokError "empty"),
      "Test tryBuildInstructionList with single ParamList"
        ~: tryBuildInstructionList [A (ASTNodeParamList [ASTNodeError (TokenInfo TokError "param")])]
        ~?= ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "param")],
      "Test tryBuildInstructionList with single InstructionSequence"
        ~: tryBuildInstructionList [A (ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "inst")])]
        ~?= ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "inst")],
      "Test tryBuildInstructionList with append InstructionSequence"
        ~: tryBuildInstructionList [A (ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "inst")]), A (ASTNodeError (TokenInfo TokError "newinst"))]
        ~?= ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "inst"), ASTNodeError (TokenInfo TokError "newinst")],
      "Test tryBuildInstructionList with two Instructions"
        ~: tryBuildInstructionList [A (ASTNodeError (TokenInfo TokError "inst1")), A (ASTNodeError (TokenInfo TokError "inst2"))]
        ~?= ASTNodeInstructionSequence [ASTNodeError (TokenInfo TokError "inst1"), ASTNodeError (TokenInfo TokError "inst2")]
    ]

testStrToAST :: Test
testStrToAST = TestList [
    -- (+ 123 678)
    "build str to ast sum" ~: strToAST "(+ 123 678)" ~?= ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678],
    -- (+ (+ 123) 2)
    "build str to ast invalid" ~: strToAST "(+ (+ 123) 2)" ~?= ASTNodeError (TokenInfo TokError "[(,+,(,+,(int: 123),),(int: 2),)]"),
    -- Define keyword taking everything as array (Needs to be fix)
    -- (define foo 123)
    -- "declare var foo with value 123" ~: strToAST "(mutable foo 123)" ~?= ASTNodeMutable (ASTNodeSymbol "foo") (ASTNodeArray [(ASTNodeInteger 123)]),
    "declare function foo with value (+ 1 2)" ~: strToAST "(define foo (+ 1 2))" ~?= ASTNodeDefine (ASTNodeSymbol "foo") (Invalid ("foo" ++ " does not take any parameters")) [(ASTNodeSum [ASTNodeInteger 1, ASTNodeInteger 2])],
    "build if" ~: strToAST "(if #t 1 2)" ~?= ASTNodeIf (ASTNodeBoolean True) [(ASTNodeInteger 1)] (Valid [(ASTNodeInteger 2)])
    ]
    -- "declare function foo with parameters (a b) and value (+ a b)" ~: strToAST "(define foo (a b) (+ a b))" ~?= ASTNodeDefine (ASTNodeSymbol "foo") (Valid (ASTNodeParamList [ASTNodeSymbol "a", ASTNodeSymbol "b"])) (ASTNodeSum [ASTNodeSymbol "a", ASTNodeSymbol "b"])]

testShowInstanceASTNode :: Test
testShowInstanceASTNode = TestList
  [
    "ASTNodeSymbol" ~: show (ASTNodeSymbol "JAja") ~?= "(sym: JAja)",
    "ASTNodeMutable" ~: show (ASTNodeMutable (ASTNodeType GInt) (ASTNodeSymbol "var") (ASTNodeType GInt) (ASTNodeInteger 4)) ~?= "(mutable: \n\t(symbol type) " ++ show (ASTNodeType GInt)  ++ "\n\t(symbol name)" ++ show  (ASTNodeSymbol "var") ++ "\n\t(value type)" ++ show (ASTNodeType GInt) ++ " \n\t(value)" ++ show (ASTNodeInteger 4) ++ ")\n",
    "ASTNodeSum" ~: show (ASTNodeSum [ASTNodeInteger 3, ASTNodeInteger 4]) ~?= "(add: " ++ show [ASTNodeInteger 3, ASTNodeInteger 4]  ++ ")",
    "ASTNodeSub" ~: show (ASTNodeSub [ASTNodeInteger 3, ASTNodeInteger 4]) ~?= "(sub: " ++ show [ASTNodeInteger 3, ASTNodeInteger 4]  ++ ")",
    "ASTNodeMul" ~: show (ASTNodeMul [ASTNodeInteger 3, ASTNodeInteger 4]) ~?= "(mul: " ++ show [ASTNodeInteger 3, ASTNodeInteger 4]  ++ ")",
    "ASTNodeDiv" ~: show (ASTNodeDiv [ASTNodeInteger 3, ASTNodeInteger 4]) ~?= "(div: " ++ show [ASTNodeInteger 3, ASTNodeInteger 4]  ++ ")",
    "ASTNodeMod" ~: show (ASTNodeMod [ASTNodeInteger 3, ASTNodeInteger 4]) ~?= "(mod: " ++ show [ASTNodeInteger 3, ASTNodeInteger 4]  ++ ")",
    "ASTNodeDebug" ~: show (ASTNodeDebug []) ~?= "(debug: [])",
    "ASTNodeParamList" ~: show (ASTNodeParamList []) ~?= "(paramlist: \n\t[])\n",
    "ASTNodeArray" ~: show (ASTNodeArray []) ~?= "(array: {\n\t[]\n})",
    "ASTNodeInstructionSequence" ~: show (ASTNodeInstructionSequence []) ~?= "(instructionsequence: \n\t[])",
    "ASTNodeBoolean" ~: show (ASTNodeBoolean True) ~?= "(bool: " ++ show True ++ ")",
    "ASTNodeIf" ~: show (ASTNodeIf (ASTNodeBoolean True) [ASTNodeInteger 3] (Valid [ASTNodeInteger 3])) ~?= "(if: \n\t(condition) " ++ show (ASTNodeBoolean True) ++ "\n\t(then) " ++ show [ASTNodeInteger 3] ++ "\n\t(else) " ++ show (Valid [ASTNodeInteger 3]) ++ ")",
    "ASTNodeElif" ~: show (ASTNodeElif (ASTNodeBoolean True) [ASTNodeInteger 3] (Valid [ASTNodeInteger 3])) ~?= "(elsif: \n\t(condition) " ++ show (ASTNodeBoolean True) ++ "\n\t(then) " ++ show [ASTNodeInteger 3] ++ "\n\t(else) " ++ show (Valid [ASTNodeInteger 3]) ++ ")",
    "ASTNodeElse" ~: show (ASTNodeElse [ASTNodeInteger 3]) ~?= "(else: \n\t(then) " ++ show [ASTNodeInteger 3] ++ ")",
    "ASTNodeDefine" ~: show (ASTNodeDefine (ASTNodeSymbol "test") (Valid (ASTNodeParamList [])) [ASTNodeInteger 6]) ~?= "(define: \n\t(name) " ++ show (ASTNodeSymbol "test") ++ "\n\t(params) " ++ show (Valid (ASTNodeParamList [])) ++ "\n\t(body) {" ++ show [ASTNodeInteger 6] ++ "})\n",
    "ASTNodePrint" ~: show (ASTNodePrint (ASTNodeInteger 4)) ~?= "(print " ++ show (ASTNodeInteger 4) ++ ")",
    "ASTNodeFunctionCall" ~: show (ASTNodeFunctionCall "test" []) ~?= "(functioncall: \n\t(name) " ++ "test" ++ "\n\t(params) [])\n",
    "ASTNodeLambda" ~: show (ASTNodeLambda (ASTNodeSymbol "lam") (Valid (ASTNodeInteger 4)) []) ~?= "(lambda: \n\t(name) " ++ show (ASTNodeSymbol "lam") ++ "\n\t(params) " ++ show (Valid (ASTNodeInteger 4)) ++ "\n\t(body) {[]})\n",
    "ASTNodeCapture" ~: show (ASTNodeCapture []) ~?= "(capture: [])",
    "ASTNodeBreak" ~: show (ASTNodeBreak []) ~?= "(break: [])",
    "ASTNodeEq" ~: show (ASTNodeEq []) ~?= "(eq: [])",
    "ASTNodeInferior" ~: show (ASTNodeInferior []) ~?= "(inferior: [])",
    "ASTNodeWhile" ~: show (ASTNodeWhile (ASTNodeBoolean True) []) ~?= "(while: \n\t(condition) " ++ show (ASTNodeBoolean True) ++ "\n\t(then) [])",
    "ASTNodeSet" ~: show (ASTNodeSet (ASTNodeSymbol "name") (ASTNodeInteger 3)) ~?= "(set: \n\t(name) " ++ show (ASTNodeSymbol "name") ++ "\n\t(children) " ++ show (ASTNodeInteger 3) ++ ")",
    "ASTNodeVariable" ~: show (ASTNodeVariable (ASTNodeSymbol "name") GInt) ~?= "(variable: \n\t(name) " ++ show (ASTNodeSymbol "name") ++ "\n\t(type) " ++ show GInt ++ ")\n",
    "ASTNodeReturn" ~: show (ASTNodeReturn (ASTNodeInteger 2)) ~?= "(return: " ++ show (ASTNodeInteger 2) ++ ")",
    "ASTNodeCast" ~: show (ASTNodeCast (ASTNodeInteger 4) GInt) ~?=  "(cast: \n\t(castee) " ++ show (ASTNodeInteger 4) ++ "\n\t(type) " ++ show GInt ++ ")",
    "ASTNodeDeref" ~: show (ASTNodeDeref (ASTNodeSymbol "name") (ASTNodeInteger 4)) ~?= "(deref: \n\t(name) " ++ show (ASTNodeSymbol "name") ++ "\n\t(index) " ++ show (ASTNodeInteger 4) ++ ")",
    "ASTNodeShow" ~: show (ASTNodeShow [] GInt) ~?= "(show: \n\t(type) " ++ show GInt ++ "\n\t(children) [])",
    "ASTNodeBinOps" ~: show (ASTNodeBinOps []) ~?= "(binops: [])",
    "ASTSuperior Unknown" ~: show (ASTNodeSuperior []) ~?= "(unknown node)"
  ]

moreTestsTokOrExprToNode :: Test
moreTestsTokOrExprToNode = TestList [
    "astnodeelif 1" ~: tokOrExprToASTNode  [T (TokenInfo TokenKeywordElse ""), T (TokenInfo TokOpenCurrBrac ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseCurrBrac "")] ~?= ASTNodeElse [ASTNodeInteger 1],
    "astnodeelif 2" ~: tokOrExprToASTNode [T (TokenInfo TokenElif ""), A (ASTNodeArray [ASTNodeBoolean True]) , T (TokenInfo TokOpenCurrBrac ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseCurrBrac "")] ~?= ASTNodeElif (head [ASTNodeBoolean True]) [ASTNodeInteger 1] (Invalid "2"),
    "astnodeelif 3" ~: tokOrExprToASTNode [T (TokenInfo TokenElif ""), T (TokenInfo TokOpenParen ""), A (ASTNodeBoolean True), T (TokenInfo TokCloseParen ""), T (TokenInfo TokOpenCurrBrac ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseCurrBrac "")] ~?= ASTNodeElif (ASTNodeBoolean True) [ASTNodeInteger 1] (Invalid "1"),
    "astnodeelif 4" ~: tokOrExprToASTNode [T (TokenInfo TokenKeywordIf ""), A (ASTNodeArray [ASTNodeBoolean True]), T (TokenInfo TokOpenCurrBrac ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseCurrBrac "")] ~?= ASTNodeIf (head [ASTNodeBoolean True]) [ASTNodeInteger 1] (Invalid "1"),
    "astnodeelif 5" ~: tokOrExprToASTNode [T (TokenInfo TokenKeywordIf ""), A (ASTNodeBoolean True), T (TokenInfo TokOpenCurrBrac ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseCurrBrac "")] ~?= ASTNodeIf (ASTNodeBoolean True) [ASTNodeInteger 1] (Invalid "2"),
    "astnodewhile 1" ~: tokOrExprToASTNode [T (TokenInfo TokenKeywordWhile ""), A (ASTNodeBoolean True), T (TokenInfo TokOpenCurrBrac ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseCurrBrac "")] ~?= ASTNodeWhile (ASTNodeBoolean True) [ASTNodeInteger 1],
    -- "astnodewhile 2" ~: tokOrExprToASTNode [T (TokenInfo TokenKeywordWhile ""), A (ASTNodeArray [ASTNodeBoolean True]), T (TokenInfo TokOpenCurrBrac ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseCurrBrac "")] ~?= ASTNodeWhile (head [ASTNodeBoolean True]) [ASTNodeInteger 1],
    "astnodeset 1" ~: tokOrExprToASTNode [A (ASTNodeSymbol "bob"), T (TokenInfo TokenEq ""), A (ASTNodeInteger 1), T (TokenInfo TokenPointComma "")] ~?= ASTNodeSet (ASTNodeSymbol "bob") (ASTNodeInteger 1),
    "astnodeset 2" ~: tokOrExprToASTNode [A (ASTNodeSymbol "bob"), T (TokenInfo TokOperatorPlus ""), T (TokenInfo TokOperatorPlus ""), T (TokenInfo TokenPointComma "")] ~?= ASTNodeSet (ASTNodeSymbol "bob") (ASTNodeSum [ASTNodeSymbol "bob", ASTNodeInteger 1]),
    "astnodeset 3" ~: tokOrExprToASTNode [A (ASTNodeSymbol "bob"), T (TokenInfo TokOperatorMinus ""), T (TokenInfo TokOperatorMinus ""), T (TokenInfo TokenPointComma "")] ~?= ASTNodeSet (ASTNodeSymbol "bob") (ASTNodeSub [ASTNodeSymbol "bob", ASTNodeInteger 1]),
    "astnode merge binop 1" ~:tokOrExprToASTNode [A (ASTNodeInteger 1), T (TokenInfo TokOperatorMul ""), A (ASTNodeInteger 2)] ~?=  mergeBinOps [A (ASTNodeInteger 1), T (TokenInfo TokOperatorMul ""), A (ASTNodeInteger 2)],
    "astnode merge binop 2" ~: tokOrExprToASTNode [A (ASTNodeInteger 1), T (TokenInfo TokOperatorDiv ""), A (ASTNodeInteger 2)] ~?=  mergeBinOps [A (ASTNodeInteger 1), T (TokenInfo TokOperatorDiv ""), A (ASTNodeInteger 2)],
    "astnode merge binop 3" ~: tokOrExprToASTNode [A (ASTNodeInteger 1), T (TokenInfo TokOperatorMod ""), A (ASTNodeInteger 2)] ~?=  mergeBinOps [A (ASTNodeInteger 1), T (TokenInfo TokOperatorMod ""), A (ASTNodeInteger 2)],
    "astnode merge binop 4" ~: tokOrExprToASTNode [A (ASTNodeInteger 1), T (TokenInfo TokOperatorPlus ""), A (ASTNodeInteger 2)] ~?= mergeBinOps [A (ASTNodeInteger 1), T (TokenInfo TokOperatorPlus ""), A (ASTNodeInteger 2)],
    "astnode merge binop 5" ~: tokOrExprToASTNode [A (ASTNodeInteger 1), T (TokenInfo TokOperatorMinus ""), A (ASTNodeInteger 2)] ~?= mergeBinOps [A (ASTNodeInteger 1), T (TokenInfo TokOperatorMinus ""), A (ASTNodeInteger 2)],
    "astnode print 1" ~: tokOrExprToASTNode [T (TokenInfo TokenSymPrint ""), T (TokenInfo TokOpenParen ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseParen ""), T (TokenInfo TokenPointComma "")] ~?= ASTNodePrint (ASTNodeInteger 1),
    "astnode print 2" ~: tokOrExprToASTNode [T (TokenInfo TokenSymPrint ""), T (TokenInfo TokOpenParen ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseParen "")] ~?= ASTNodePrint (ASTNodeInteger 1),
    "astnode print 3" ~: tokOrExprToASTNode [A (ASTNodePrint (ASTNodeInteger 1)), T (TokenInfo TokenPointComma "")] ~?= ASTNodePrint (ASTNodeInteger 1),
    "astnode ineq " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), A (ASTNodeInteger 1), T (TokenInfo TokenInferior ""), A (ASTNodeInteger 2), T (TokenInfo TokCloseParen "")] ~?= ASTNodeInferior [ASTNodeInteger 1, (ASTNodeInteger 2)],
    "astnode ineq " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), A (ASTNodeInteger 1), T (TokenInfo TokenInferiorEq ""), A (ASTNodeInteger 2), T (TokenInfo TokCloseParen "")] ~?= ASTNodeInferiorEq [ASTNodeInteger 1, (ASTNodeInteger 2)],
    "astnode ineq " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), A (ASTNodeInteger 1), T (TokenInfo TokenSuperior ""), A (ASTNodeInteger 2), T (TokenInfo TokCloseParen "")] ~?= ASTNodeSuperior [ASTNodeInteger 1, (ASTNodeInteger 2)],
    "astnode ineq " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), A (ASTNodeInteger 1), T (TokenInfo TokenSuperiorEq ""), A (ASTNodeInteger 2), T (TokenInfo TokCloseParen "")] ~?= ASTNodeSuperiorEq [ASTNodeInteger 1, (ASTNodeInteger 2)],
    "astnode ineq " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), A (ASTNodeInteger 1), T (TokenInfo TokenEqual ""), A (ASTNodeInteger 2), T (TokenInfo TokCloseParen "")] ~?= ASTNodeEq [ASTNodeInteger 1, (ASTNodeInteger 2)],
    "astnode ineq " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), A (ASTNodeInteger 1), T (TokenInfo TokenNotEqual ""), A (ASTNodeInteger 2), T (TokenInfo TokCloseParen "")] ~?= ASTNodeNotEqual [ASTNodeInteger 1, ASTNodeInteger 2],
    "astnode mutable " ~: tokOrExprToASTNode [A (ASTNodeVariable (ASTNodeSymbol "bob") GInt), T (TokenInfo TokenEq ""), A (ASTNodeInteger 1), T (TokenInfo TokenPointComma "")] ~?= ASTNodeMutable (ASTNodeType GInt) (ASTNodeSymbol "bob") (ASTNodeType (getTypeFromNodeValue (ASTNodeInteger 1))) (ASTNodeInteger 1),
    "astnode mutable " ~: tokOrExprToASTNode [A (ASTNodeMutable (ASTNodeType GInt) (ASTNodeSymbol "bob") (ASTNodeType GInt) (ASTNodeInteger 1)), T (TokenInfo TokenPointComma "")] ~?= ASTNodeMutable (ASTNodeType GInt)  (ASTNodeSymbol "bob") (ASTNodeType GInt) (ASTNodeInteger 1),
    "astnode mutable " ~: tokOrExprToASTNode [A (ASTNodeVariable (ASTNodeSymbol "bob") GInt), T (TokenInfo TokenEq ""), A (ASTNodeInteger 1)] ~?= ASTNodeMutable (ASTNodeType GInt) (ASTNodeSymbol "bob") (ASTNodeType (getTypeFromNodeValue (ASTNodeInteger 1))) (ASTNodeInteger 1),
    -- "astnode function call " ~: tokOrExprToASTNode [A (ASTNodeSymbol "bob"), T (TokenInfo TokOpenParen ""), A (ASTNodeParamList [ASTNodeInteger 1]), T (TokenInfo TokCloseParen ""), T (TokenInfo TokenPointComma "")] ~?= ASTNodeFunctionCall (ASTNodeSymbol "bob") [ASTNodeInteger 1],
    "astnode function call " ~: tokOrExprToASTNode [A (ASTNodeSymbol "bob"), T (TokenInfo TokOpenParen ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseParen ""), T (TokenInfo TokenPointComma "")] ~?= ASTNodeFunctionCall "bob" [ASTNodeInteger  1],
    "ast node define " ~: tokOrExprToASTNode [A (ASTNodeVariable (ASTNodeSymbol "bob") GInt), T (TokenInfo TokOpenParen ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseParen ""), T (TokenInfo TokOpenCurrBrac ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseCurrBrac "")] ~?= ASTNodeDefine (ASTNodeSymbol "bob") (Valid (ASTNodeParamList [ASTNodeInteger 1])) [ASTNodeInteger 1],
    "ast node define " ~: tokOrExprToASTNode [A (ASTNodeParamList [ASTNodeVariable (ASTNodeSymbol "bob") GInt, ASTNodeArray [ASTNodeInteger 1]]), T (TokenInfo TokOpenCurrBrac ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseCurrBrac "")] ~?= ASTNodeDefine (ASTNodeSymbol "bob") (Valid (ASTNodeParamList [ASTNodeInteger 1])) [ASTNodeInteger 1],
    -- "ast node define " ~: tokOrExprToASTNode [A (ASTNodeVariable (ASTNodeSymbol "bob") GInt), A (ASTNodeArray [ASTNodeInteger 1]), T (TokenInfo TokOpenCurrBrac ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseCurrBrac "")] ~?= ASTNodeDefine (ASTNodeSymbol "bob") (Valid (ASTNodeParamList ASTNodeInteger 1)) [ASTNodeInteger 1],
    "astnode node capture " ~: tokOrExprToASTNode [T (TokenInfo TokOpenBrac ""), T (TokenInfo TokCloseBrac "")] ~?= ASTNodeCapture [],
    "astnode return " ~: tokOrExprToASTNode [T (TokenInfo TokenReturn ""), A (ASTNodeInteger 1), T (TokenInfo TokenPointComma "")] ~?= ASTNodeReturn (ASTNodeInteger 1),
    "another variable " ~: tokOrExprToASTNode [T (TokenInfo TokenType "int"), A (ASTNodeSymbol "bob")] ~?= ASTNodeVariable (ASTNodeSymbol "bob") (getTypeFromToken (TokenInfo TokenType "int")),
    "negative integer " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokOperatorMinus ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseParen "")] ~?= ASTNodeInteger (-1),
    "function call with simbol and integer " ~: tokOrExprToASTNode [A (ASTNodeArray [ASTNodeSymbol "bob", ASTNodeInteger 1])] ~?= ASTNodeFunctionCall "bob" [ASTNodeInteger 1],
    "lambda with variable " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokLambda ""), A (ASTNodeParamList [ASTNodeFunctionCall "bob" [ASTNodeInteger 2], ASTNodeInteger 3]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeLambda (ASTNodeSymbol "") (Valid (ASTNodeParamList (ASTNodeSymbol "bob" : [ASTNodeInteger 2]))) [ASTNodeInteger 3],
    -- "define with params " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokKeywordDefine ""), A (ASTNodeSymbol "bob"), A (ASTNodeLambda (ASTNodeSymbol "bob") (ASTNodeInteger 1) (ASTNodeInteger 1)), T (TokenInfo TokCloseParen "")] ~?= ASTNodeDefine (ASTNodeSymbol "bob") (Valid (ASTNodeInteger 1)) (expendParamList ASTNodeInteger 1),
    -- "ASTNode define 32" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokKeywordDefine ""), A (ASTNodeSymbol "bob"), A (ASTNodeFunctionCall "foo" [ASTNodeInteger 1]), A (ASTNodeArray [ASTNodeInteger 2]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeDefine (ASTNodeSymbol "bob") (Valid (ASTNodeParamList (ASTNodeSymbol "bob" : [ASTNodeInteger 1]))) (ASTNodeInteger 2),
    "ASTNode define 32" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokKeywordDefine ""), A (ASTNodeParamList [ASTNodeSymbol "bob", ASTNodeFunctionCall "foo" [ASTNodeInteger 1], (ASTNodeInteger 2)]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeDefine (ASTNodeSymbol "bob") (Valid (ASTNodeParamList (ASTNodeSymbol "foo" : [ASTNodeInteger 1]))) [ASTNodeInteger 2],
    "ASTNode define 32" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokKeywordDefine ""), A (ASTNodeParamList [ASTNodeFunctionCall "foo" [ASTNodeInteger 1], (ASTNodeInteger 2)]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeDefine (ASTNodeSymbol "foo") (Valid (ASTNodeParamList [ASTNodeInteger 1])) [ASTNodeInteger 2],
    "ast node if " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokenKeywordIf ""), A (ASTNodeParamList [ASTNodeBoolean True, (ASTNodeInteger 1), (ASTNodeInteger 2)]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeIf (ASTNodeBoolean True) [(ASTNodeInteger 1)] (Valid [(ASTNodeInteger 2)]),
    "ast node if " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokenKeywordIf ""), A (ASTNodeParamList [ASTNodeBoolean True, ASTNodeParamList [(ASTNodeInteger 1), (ASTNodeInteger 2)]]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeIf (ASTNodeBoolean True) [(ASTNodeInteger 1)] (Valid [(ASTNodeInteger 2)]),
    "ast node if 3 " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokenKeywordIf ""), A (ASTNodeArray [ASTNodeBoolean True]), A (ASTNodeArray [ASTNodeInteger 1]), A (ASTNodeArray [ASTNodeInteger 2]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeIf (head [ASTNodeBoolean True]) [ASTNodeInteger 1] (Valid [ASTNodeInteger 2]),
    "ast node anot" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokenKeywordIf ""), A (ASTNodeArray [ASTNodeBoolean True]), A (ASTNodeInteger 1), A (ASTNodeInteger 2), T (TokenInfo TokCloseParen "")] ~?= ASTNodeIf (head [ASTNodeBoolean True]) [ASTNodeInteger 1] (Valid [ASTNodeInteger 2]),
    "ast node anot" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokenKeywordIf ""), A (ASTNodeArray [ASTNodeBoolean True]), T (TokenInfo TokenKeywordThen ""), A (ASTNodeInteger 1), T (TokenInfo TokCloseParen "")] ~?= ASTNodeIf (head [ASTNodeBoolean True]) [ASTNodeInteger 1] (Invalid "3"),
    "ast node if 238" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokenKeywordIf ""), A (ASTNodeBoolean True), T (TokenInfo TokenKeywordThen ""), A (ASTNodeInteger 1), T (TokenInfo TokenKeywordElse ""), A (ASTNodeInteger 2), T (TokenInfo TokCloseParen "")] ~?= ASTNodeIf (ASTNodeBoolean True) (expendParamList [ASTNodeInteger 1]) (Valid (expendParamList [ASTNodeInteger 2])),
    "ast node inf ineq " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokenInferior ""), A (ASTNodeParamList [(ASTNodeInteger 1), (ASTNodeInteger 2)]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeInferior [(ASTNodeInteger 1), (ASTNodeInteger 2)],
    "ast node inf ineq " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokenInferiorEq ""), A (ASTNodeParamList [(ASTNodeInteger 1), (ASTNodeInteger 2)]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeInferiorEq [(ASTNodeInteger 1), (ASTNodeInteger 2)],
    "ast node inf ineq " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokenSuperior ""), A (ASTNodeParamList [(ASTNodeInteger 1), (ASTNodeInteger 2)]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeSuperior [(ASTNodeInteger 1), (ASTNodeInteger 2)],
    "ast node sup ineq " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokenSuperiorEq ""), A (ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2]), T (TokenInfo TokCloseParen "")] ~?= ASTNodeSuperiorEq [ASTNodeInteger 1, ASTNodeInteger 2],
    "astnode  mutable get type " ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen ""), T (TokenInfo TokKeywordMutable ""), A (ASTNodeSymbol "bob"), A (ASTNodeInteger 1), T (TokenInfo TokCloseParen "")] ~?= ASTNodeMutable (ASTNodeType (getTypeFromNodeValue (ASTNodeInteger 1))) (ASTNodeSymbol "bob") (ASTNodeType (getTypeFromNodeValue (ASTNodeInteger 1))) (ASTNodeInteger 1),
    "ast node error " ~: tokOrExprToASTNode [A (ASTNodeIf (ASTNodeBoolean True) [ASTNodeInteger 1] (Invalid "no else")), A (ASTNodeElif (ASTNodeBoolean True) [] (Invalid "no else"))] ~?= ASTNodeError (TokenInfo TokError "cannot resolve input"),
    "ast node error " ~: tokOrExprToASTNode [A (ASTNodeElif (ASTNodeBoolean True) [ASTNodeInteger 1] (Invalid "no else")), A (ASTNodeElif (ASTNodeBoolean True) [] (Invalid "no else"))] ~?= ASTNodeError (TokenInfo TokError "cannot resolve input"),
    "ast node error " ~: tokOrExprToASTNode [A (ASTNodeSet (ASTNodeBoolean True) (ASTNodeInteger 1)), A (ASTNodeSymbol "")] ~?= ASTNodeError (TokenInfo TokError "cannot resolve input"),
    -- "ast node error " ~: tokOrExprToASTNode [A (ASTNodeInteger 1), A (ASTNodeVariable  (ASTNodeInteger 1) Gint)] ~?= ASTNodeError (TokenInfo TokError "cannot resolve input"),
    -- "ast node error " ~: tokOrExprToASTNode [A (ASTNodeVariable "" ""), A (ASTNodeArray "")] ~?= ASTNodeError (TokenInfo TokError "cannot resolve input"),
    -- "ast node error " ~: tokOrExprToASTNode [A (ASTNodeReturn ""), A (ASTNodeInteger 1)] ~?= ASTNodeError (TokenInfo TokError "cannot resolve input"),
    -- "astnode param list" ~: tokOrExprToASTNode [A (ASTNodeInteger 1), T (TokenInfo TokenComma _), A (ASTNodeInteger 2)] ~?= ASTNodeParamList [(ASTNodeInteger 1), (ASTNodeInteger 2)],
    -- "astnode param list" ~: tokOrExprToASTNode [A (ASTNodeParamList [ASTNodeInteger 1]), T (TokenInfo TokenComma ""), A (ASTNodeInteger 2)] ~?= ASTNodeParamList ([ASTNodeInteger 1] ++ [AstNodeInteger 2]),
    "show with cast 1" ~: tokOrExprToASTNode [T (TokenInfo TokenShowKeyword ""), A (ASTNodeParamList [ASTNodeInteger 1]), T (TokenInfo TokenAsKeyword ""), T (TokenInfo TokenType "int"), T (TokenInfo TokenPointComma "")] ~?= ASTNodeShow [ASTNodeInteger 1] (getTypeFromToken (TokenInfo TokenType "int")),
    "show with cast 2" ~: tokOrExprToASTNode [T (TokenInfo TokenShowKeyword ""), A (ASTNodeArray [ASTNodeInteger 1]), T (TokenInfo TokenAsKeyword ""), T (TokenInfo TokenType "int"), T (TokenInfo TokenPointComma "")] ~?= ASTNodeShow [ASTNodeInteger 1] (getTypeFromToken (TokenInfo TokenType "int")),
    "show with cast 3" ~: tokOrExprToASTNode [T (TokenInfo TokenShowKeyword ""), A (ASTNodeInteger 1), T (TokenInfo TokenAsKeyword ""), T (TokenInfo TokenType "int"), T (TokenInfo TokenPointComma "")] ~?= ASTNodeShow [ASTNodeInteger 1] (getTypeFromToken (TokenInfo TokenType "int")),
    "show with cast 4" ~: tokOrExprToASTNode [T (TokenInfo TokenShowKeyword ""), A (ASTNodeBoolean True), T (TokenInfo TokenAsKeyword ""), T (TokenInfo TokenType "int"), T (TokenInfo TokenPointComma "")] ~?= ASTNodeShow [ASTNodeBoolean True] (getTypeFromToken (TokenInfo TokenType "int")),
    "show with cast 5" ~: tokOrExprToASTNode [T (TokenInfo TokenShowKeyword ""), A (ASTNodeSymbol "bob"), T (TokenInfo TokenAsKeyword ""), T (TokenInfo TokenType "int"), T (TokenInfo TokenPointComma "")] ~?= ASTNodeShow [ASTNodeSymbol "bob"] (getTypeFromToken (TokenInfo TokenType "int")),
    "ast node cst " ~: tokOrExprToASTNode [A (ASTNodeInteger 1), T (TokenInfo TokenCast ""), T (TokenInfo TokenType "int")] ~?= ASTNodeCast (ASTNodeInteger 1) (getTypeFromToken (TokenInfo TokenType "int")),
    "ast node deref " ~: tokOrExprToASTNode [A (ASTNodeInteger 1), T (TokenInfo TokenDeref ""), A (ASTNodeInteger 2)] ~?= ASTNodeDeref (ASTNodeInteger 1) (ASTNodeInteger 2),
    "ast node show simple " ~: tokOrExprToASTNode [T (TokenInfo TokenShowKeyword ""), A (ASTNodeCast (ASTNodeInteger 1) GInt) , T (TokenInfo TokenPointComma "")] ~?= ASTNodeShow [ASTNodeInteger 1] GInt,
    "type to int undefined - 1" ~: typeToInt GUndefinedType ~?= 1,
    "type to int int - 2" ~: typeToInt GInt ~?= 2,
    "type to int bool - 3" ~: typeToInt GBool ~?= 3,
    -- "type to int void - 4" ~: typeToInt Gvoid ~?= 4,
    "type to int ptr - 5" ~: typeToInt GPtr ~?= 5,
    -- "int to type undefined - 1" ~: intToType (Valid 1) ~?= GUndefinedType,
    -- "int to type int - 2" ~: intToType (Valid 2) ~?= GInt,
    -- "int to type bool - 3" ~: intToType (Valid 3) ~?= GBool,
    -- "int to type void - 4" ~: intToType (Valid 4) ~?= Gvoid,
    -- "int to type ptr - 5" ~: intToType (Valid 5) ~?= GPtr,
    "int to type invalid" ~: intToType (Invalid "non") ~?= Invalid "non",
    "int to type random val " ~: intToType (Valid 300) ~?= Invalid "invalid type",
    "expand param list" ~: expandParamLists ((ASTNodeParamList [ASTNodeInteger 1]) : [ASTNodeInteger 1]) ~?= [ASTNodeInteger 1] ++ expandParamLists [ASTNodeInteger 1],
    "expand param list" ~: expandParamLists (ASTNodeInteger 1 : [ASTNodeInteger 2]) ~?= (ASTNodeInteger 1) : expandParamLists [ASTNodeInteger 2],
    "expand param list" ~: expandParamLists [] ~?= []
    -- "contains param list " ~:  containsParamList [ASTNodeInteger 1, ASTNodeParamList [ASTNodeInteger 2]] ~?= True,
    -- "check check" ~: check ASTNodeInstructionSequence ([ASTNodeInteger 1, ASTNodeParamList [ASTNodeInteger 2]]) ~?= ASTNodeInstructionSequence [ASTNodeInteger 1, ASTNodeInteger 2]
    ]

testIsSymbolAndParamArray :: Test
testIsSymbolAndParamArray = TestList
  [
    "ASTNodeSymbol" ~: isSymbolAndParamArray [ASTNodeSymbol "String", ASTNodeInteger 4, ASTNodeInteger 3] ~?= True,
    "AstNodeLambda" ~: isSymbolAndParamArray [ASTNodeLambda (ASTNodeSymbol "String") (Invalid "Error") [], ASTNodeInteger 3] ~?= True
  ]

moreTestFunctions :: Test
moreTestFunctions = TestList
  [
    "expendParamList Array" ~: expendParamList [ASTNodeArray [], ASTNodeInteger 4] ~?= expendParamList [] ++ expendParamList [ASTNodeInteger 4],
    "expendParamList ParamList" ~: expendParamList [ASTNodeParamList [], ASTNodeInteger 4] ~?= expendParamList [] ++ expendParamList [ASTNodeInteger 4],
    "isThisReallyAnArrayOrIsItATrap" ~: isThisReallyAnArrayOrIsItATrap (ASTNodeError (TokenInfo {token = TokEmpty, value = ""})) ~?= ASTNodeError (TokenInfo {token = TokEmpty, value = ""}),
    "getTypeFromToken" ~: getTypeFromToken (TokenInfo TokenType "bool") ~?= GBool,
    "getTypeFromToken" ~: getTypeFromToken (TokenInfo TokenType "int") ~?= GInt,
    "getTypeFromToken" ~: getTypeFromToken (TokenInfo TokenType "void") ~?= GVoid,
    "getTypeFromToken" ~: getTypeFromToken (TokenInfo TokenType "@") ~?= GPtr,
    "getTypeFromToken" ~: getTypeFromToken (TokenInfo TokenType "undefined") ~?= GUndefinedType,
    "getTypeFromToken" ~: getTypeFromToken (TokenInfo TokenType "bob") ~?= GUndefinedType,
    "getTypeFromNodeValue" ~: getTypeFromNodeValue (ASTNodeInteger 3) ~?= GInt,
    "getTypeFromNodeValue" ~: getTypeFromNodeValue (ASTNodeBoolean True) ~?= GBool,
    "getTypeFromNodeValue" ~: getTypeFromNodeValue (ASTNodeSymbol "bob") ~?= GUndefinedType,
    "getTypeFromNodeValue" ~: getTypeFromNodeValue (ASTNodeArray []) ~?= GPtr,
    "getTypeFromNodeValue" ~: getTypeFromNodeValue (ASTNodeCast (ASTNodeInteger 4) GInt) ~?= GInt,
    "mergeBinOps" ~: mergeBinOps [A (ASTNodeBinOps [])] ~?= ASTNodeBinOps [],
    "mergeBinOps" ~: mergeBinOps [] ~?= ASTNodeError (TokenInfo TokError "Invalid binary operation: []"),
    "mergeBinOps" ~: mergeBinOps [A (ASTNodeBinOps [A (ASTNodeInteger 3)]), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeBinOps [A (ASTNodeInteger 3)])] ~?= ASTNodeBinOps [A (ASTNodeInteger 3), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeInteger 3)],
    "mergeBinOps" ~: mergeBinOps [A (ASTNodeBinOps [A (ASTNodeInteger 3)]), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeInteger 2)] ~?= ASTNodeBinOps [A (ASTNodeInteger 3), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeInteger 2)],
    "mergeBinOps" ~: mergeBinOps [A (ASTNodeInteger 3), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeBinOps [A (ASTNodeInteger 3)])] ~?= ASTNodeBinOps [A (ASTNodeInteger 3), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeInteger 3)],
    "typeToInt" ~: typeToInt GUndefinedType ~?= 1,
    "typeToInt" ~: typeToInt GInt ~?= 2,
    "typeToInt" ~: typeToInt GBool ~?= 3,
    "typeToInt" ~: typeToInt GVoid ~?= 4,
    "typeToInt" ~: typeToInt GPtr ~?= 5,
    "intToType" ~: intToType (Valid 1) ~?= Valid GUndefinedType,
    "intToType" ~: intToType (Valid 2) ~?= Valid GInt,
    "intToType" ~: intToType (Valid 3) ~?= Valid GBool,
    "intToType" ~: intToType (Valid 4) ~?= Valid GVoid,
    "intToType" ~: intToType (Valid 5) ~?= Valid GPtr,
    "instructionSequenceExpandParamList" ~: instructionSequenceExpandParamList (ASTNodeInstructionSequence []) ~?= ASTNodeInstructionSequence (expandParamLists [])
  ]

testLexerConstructor :: Test
testLexerConstructor = TestList
  [
    "ASTNodeEq" ~: show (astneChildren ASTNodeEq { astneChildren = [] }) ~?= "[]",
    "ASTNodeMutable" ~: show ( astnmSymType (ASTNodeMutable (ASTNodeType GInt) (ASTNodeSymbol "bob") (ASTNodeType GInt) (ASTNodeInteger 3))) ~?= show (ASTNodeType GInt),
    "ASTNodeMutable" ~: show ( astnmName (ASTNodeMutable (ASTNodeType GInt) (ASTNodeSymbol "bob") (ASTNodeType GInt) (ASTNodeInteger 3))) ~?= show (ASTNodeSymbol "bob"),
    "ASTNodeMutable" ~: show ( astnmValueType (ASTNodeMutable (ASTNodeType GInt) (ASTNodeSymbol "bob") (ASTNodeType GInt) (ASTNodeInteger 3))) ~?= show (ASTNodeType GInt),
    "ASTNodeMutable" ~: show ( astnmChildren (ASTNodeMutable (ASTNodeType GInt) (ASTNodeSymbol "bob") (ASTNodeType GInt) (ASTNodeInteger 3))) ~?= show (ASTNodeInteger 3),
    "ASTNodeInferior" ~: show ( astniChildren (ASTNodeInferior [])) ~?= "[]",
    "ASTNodeIf" ~: show ( astniCondition (ASTNodeIf (ASTNodeBoolean True) [] (Valid []))) ~?= show (ASTNodeBoolean True),
    "ASTNodeIf" ~: show ( astniThen (ASTNodeIf (ASTNodeBoolean True) [] (Valid []))) ~?= "[]",
    "ASTNodeIf" ~: show ( astniElse (ASTNodeIf (ASTNodeBoolean True) [] (Valid []))) ~?= "Valid []",
    "ASTNodePrint" ~: show ( astnPrint (ASTNodePrint (ASTNodeInteger 4))) ~?= show (ASTNodeInteger 4),
    "ASTNodeDefine" ~: show ( astndParams (ASTNodeDefine (ASTNodeSymbol "ddd") (Valid (ASTNodeInteger 4)) [])) ~?= show (Valid (ASTNodeInteger 4)),
    "ASTNodeDefine" ~: show ( astndBody (ASTNodeDefine (ASTNodeSymbol "ddd") (Valid (ASTNodeInteger 4)) [])) ~?= "[]",
    "ASTNodeCapture" ~: show ( astncChildren (ASTNodeCapture [])) ~?= "[]",
    "ASTNodeFunctionCall" ~: show ( astnfName (ASTNodeFunctionCall "Name" [])) ~?= "\"Name\"",
    "ASTNodeFunctionCall" ~: show ( astfnParams (ASTNodeFunctionCall "Name" [])) ~?= "[]",
    "ASTNodeType" ~: show ( astntName (ASTNodeType GInt)) ~?= show GInt,
    "ASTNodeVariable" ~: show ( astnvName (ASTNodeVariable (ASTNodeSymbol "Var") GInt)) ~?= show (ASTNodeSymbol "Var"),
    "ASTNodeVariable" ~: show ( astnvType (ASTNodeVariable (ASTNodeSymbol "Var") GInt)) ~?= "GInt",
    "ASTNodeReturn" ~: show ( astnrValue (ASTNodeReturn (ASTNodeInteger 4))) ~?= show (ASTNodeInteger 4),
    "ASTNodeDeref" ~: show ( astndindex (ASTNodeDeref (ASTNodeInteger 5) (ASTNodeInteger 5))) ~?= show (ASTNodeInteger 5),
    "ASTNodeCast" ~: show ( astncastee (ASTNodeCast (ASTNodeSymbol "Var") GInt)) ~?= show (ASTNodeSymbol "Var"),
    "ASTNodeCast" ~: show ( astncasttype (ASTNodeCast (ASTNodeSymbol "Var") GInt)) ~?= "GInt",
    "ASTNodeShow" ~: show ( astnsType (ASTNodeShow [] GInt)) ~?= show GInt,
    "ASTNodeBinOps" ~: show ( astnboChildren (ASTNodeBinOps [])) ~?= "[]"
  ]

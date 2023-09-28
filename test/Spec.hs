import Test.HUnit
import Tokenizer
    ( tokenize,
      tryTokenizeOne,
      wordToTok,
      Token(TokError, TokEmpty, TokComment, TokOperatorDiv,
            TokOperatorMinus, TokKeyworddefine, TokSymbol, TokOpenParen,
            TokOperatorPlus, TokInteger, TokCloseParen),
      TokenInfo(TokenInfo) )
import Lexer
    ( buildAST,
      buildASTIterate,
      strToAST,
      tokOrExprToASTNode,
      tryToMatch,
      ASTNode(ASTNodeInteger, ASTNodeSum, ASTNodeError, ASTNodeDefine,
              ASTNodeSymbol, astniValue, ASTNodeParamList, ASTNodeArray),
      TokorNode(T, A) )
import VM( regGet, regSet, regInc, newContext, Register(..), regDec, regAdd,
           regSub, regMul, regDiv, regMod,
           regAnd, regOr, regXor, regNot,
           newStack, stackPush, stackPop, stackPeek, stackDup, stackSwap,
           stackRot, newHeap, heapSet, heapGet, heapAlloc, heapFree,
           newLabels, labelSet, labelGet, labelFree,
           newFlags, flagSet, flagGet, Flag(..), Instruction(..), Param(..))
import qualified Data.Maybe as Data

import Instructions


-- testTokenize :: Test
-- testTokenize = TestList [
--     "Tokenize empty string" ~: (tokenize "") ~?= [],
--     "Tokenize whitespace" ~: tokenize " " ~?= [TokWhitespace],
--     "Tokenize Symbol" ~: tokenize "abc" ~?= [TokSymbol]]
    -- "Tokenize integer" ~: tokenize "123" ~?= [TokInteger]]
    -- "Tokenize plus" ~: tokenize "+" ~?= [TokOperatorPlus],
    -- "Tokenize define" ~: tokenize "define" ~?= [TokKeyworddefine],
    -- "Tokenize comment" ~: tokenize "//" ~?= [TokComment],
    -- "Tokenize open paren" ~: tokenize "(" ~?= [TokOpenParen],
    -- "Tokenize close paren" ~: tokenize ")" ~?= [TokOpenParen],


testWordToToken :: Test
testWordToToken = TestList [
    "WordToToken empty string" ~: wordToTok "" ~?= TokenInfo TokEmpty "",
    "WordToToken Symbol" ~: wordToTok "abc" ~?= TokenInfo TokSymbol "abc",
    "WordToToken integer" ~: wordToTok "123" ~?= TokenInfo TokInteger "123",
    "WordToToken plus" ~: wordToTok "+" ~?= TokenInfo TokOperatorPlus "+",
    "WordToToken minus" ~: wordToTok "-" ~?= TokenInfo TokOperatorMinus "-",
    "WordToToken div" ~: wordToTok "/" ~?= TokenInfo TokOperatorDiv "/",
    "WordToToken define" ~: wordToTok "define" ~?= TokenInfo TokKeyworddefine "define",
    "WordToToken comment" ~: wordToTok "//" ~?= TokenInfo TokComment "//",
    "WordToToken open paren" ~: wordToTok "(" ~?= TokenInfo TokOpenParen "(",
    "WordToToken close paren" ~: wordToTok ")" ~?= TokenInfo TokCloseParen ")",
    "WordToToken error" ~: wordToTok "°" ~?= TokenInfo TokError "°"]

testTryTokenizeOne :: Test
testTryTokenizeOne = TestList [
    "TryTokenizeOne empty string" ~: tryTokenizeOne "" (TokenInfo TokError "") "" ~?= (TokenInfo TokEmpty "", []),
    "TryTokenizeOne Symbol" ~: tryTokenizeOne "" (TokenInfo TokError "") "abc" ~?= (TokenInfo TokSymbol "abc", ""),
    "TryTokenizeOne integer" ~: tryTokenizeOne "" (TokenInfo TokError "") "123" ~?= (TokenInfo TokInteger "123", ""),
    "TryTokenizeOne plus" ~: tryTokenizeOne "" (TokenInfo TokError "") "+" ~?= (TokenInfo TokOperatorPlus "+", ""),
    "TryTokenizeOne define" ~: tryTokenizeOne "" (TokenInfo TokError "") "define" ~?= (TokenInfo TokKeyworddefine "define", ""),
    "TryTokenizeOne comment" ~: tryTokenizeOne "" (TokenInfo TokError "") "//" ~?= (TokenInfo TokComment "//", ""),
    "TrytokenizeOne div" ~: tryTokenizeOne "" (TokenInfo TokError "") "/" ~?= (TokenInfo TokOperatorDiv "/", ""),
    "TryTokenizeOne open paren" ~: tryTokenizeOne "" (TokenInfo TokError "") "(" ~?= (TokenInfo TokOpenParen "(", ""),
    "TryTokenizeOne close paren" ~: tryTokenizeOne "" (TokenInfo TokError "") ")" ~?= (TokenInfo TokCloseParen ")", ""),
    "TryTokenizeOne minus" ~: tryTokenizeOne "" (TokenInfo TokError "") "-" ~?= (TokenInfo TokOperatorMinus "-", ""),
    "TryTokenizeOne error" ~: tryTokenizeOne "" (TokenInfo TokError "") "°" ~?= (TokenInfo TokError "", "°")]

testTokenize :: Test
testTokenize = TestList [
    "Tokenize empty string" ~: tokenize "" ~?= [],
    "Tokenize Symbol" ~: tokenize "abc" ~?= [TokenInfo TokSymbol "abc"],
    "Tokenize variable definition" ~: tokenize "define oui 123" ~?= [TokenInfo TokKeyworddefine "define", TokenInfo TokSymbol "oui" , TokenInfo TokInteger "123"]]

testTokOrExprToNode :: Test
testTokOrExprToNode = TestList [
    "node error" ~: tokOrExprToASTNode [] ~?= ASTNodeError (TokenInfo TokError ""),
    "node integer" ~: tokOrExprToASTNode [T (TokenInfo TokInteger "123")] ~?= ASTNodeInteger 123,
    "node sum" ~: tokOrExprToASTNode [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeInteger 1), A (ASTNodeInteger 2), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSum [ASTNodeInteger 1, ASTNodeInteger 2]]

testTryToMatch :: Test
testTryToMatch = TestList [
    "no match" ~: tryToMatch [] (T (TokenInfo TokError "")) [] ~?=  (A (ASTNodeError (TokenInfo TokError "")), []),
    "node match integer 0" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokInteger "123")] ~?= (A (ASTNodeInteger {astniValue = 123}),[]),
    "node match integer 1" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "567")] ~?= (A (ASTNodeInteger {astniValue = 123}), [T (TokenInfo TokInteger "567")]),
    "node match integer 2" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "567"), T (TokenInfo TokInteger "000")] ~?= (A (ASTNodeInteger {astniValue = 123}), [T (TokenInfo TokInteger "567"), T (TokenInfo TokInteger "000")]),
    "node match simple sum" ~: tryToMatch [] (T (TokenInfo TokError "")) [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeInteger 123), A (ASTNodeInteger 678), T (TokenInfo TokCloseParen ")")] ~?= (A (ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678]),[])]

-- try to parse the tokens that make the following expression: (+ 1 (+ 2 3))
-- the test should return a sum node that contains an integer node and another sum node
testBuildASTIterate :: Test
testBuildASTIterate = TestList [
    "build ast sum" ~: buildASTIterate [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeInteger 123), A (ASTNodeInteger 678), T (TokenInfo TokCloseParen ")")] ~?= [A (ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678])],
    "build ast integer 1" ~: buildASTIterate [T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "567")] ~?= [A (ASTNodeInteger 123) , A (ASTNodeInteger 567)],
    "build middle index" ~: buildASTIterate [T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "678"), T (TokenInfo TokCloseParen ")")] ~?= [A (ASTNodeInteger 123), A (ASTNodeInteger 678), T (TokenInfo TokCloseParen ")")],
    "incomplete iteration" ~: buildASTIterate [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "678"), T (TokenInfo TokCloseParen ")")] ~?= [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), A (ASTNodeInteger 123), A (ASTNodeInteger 678), T (TokenInfo TokCloseParen ")")]]

testBuildAST :: Test
testBuildAST = TestList [
    -- (+ 123 678)
    "build ast sum" ~: buildAST[T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "678"), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678],
    -- (+ 123 (+ 678 000))
    "build ast nested sum 1" ~: buildAST [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "678"), T (TokenInfo TokInteger "000"), T (TokenInfo TokCloseParen ")"), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSum [ASTNodeInteger 123, ASTNodeSum [ASTNodeInteger 678, ASTNodeInteger 0]],
    -- (+ (+ 123 678) 000)
    "build ast nested sum 2" ~: buildAST [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokInteger "678"), T (TokenInfo TokCloseParen ")"), T (TokenInfo TokInteger "000"), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeSum [ ASTNodeSum [ ASTNodeInteger 123, ASTNodeInteger 678], ASTNodeInteger 0],
    -- (+ (+ 123) 2)
    "build ast error invalid expr" ~: buildAST [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), T (TokenInfo TokInteger "123"), T (TokenInfo TokCloseParen ")"), T (TokenInfo TokInteger "2"), T (TokenInfo TokCloseParen ")")] ~?= ASTNodeError (TokenInfo TokError "cannot resolve input")]

testStrToAST :: Test
testStrToAST = TestList [
    -- (+ 123 678)
    "build str to ast sum" ~: strToAST "(+ 123 678)" ~?= ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678],
    -- (+ (+ 123) 2)
    "build str to ast invalid" ~: strToAST "(+ (+ 123) 2)" ~?= ASTNodeError (TokenInfo TokError "cannot resolve input"),
    -- (define foo 123)
    "declare var foo with value 123" ~: strToAST "(define foo 123)" ~?= ASTNodeDefine (ASTNodeSymbol "foo") [ASTNodeInteger 123]]

testASTNodeParamList :: Test
testASTNodeParamList = TestList [
    "test three integers: " ~: strToAST "1 2 3" ~?= ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2, ASTNodeInteger 3],
    "test two integers: " ~: strToAST "1 2" ~?= ASTNodeParamList [ASTNodeInteger 1, ASTNodeInteger 2],
    "test one integer: " ~: strToAST "1" ~?= ASTNodeInteger 1]

testASTNodeArray :: Test
testASTNodeArray = TestList [
    "test three integers: " ~: strToAST "(1 2 3)" ~?= ASTNodeArray [ASTNodeInteger 1, ASTNodeInteger 2, ASTNodeInteger 3],
    "test two integers: " ~: strToAST "(1 2)" ~?= ASTNodeArray [ASTNodeInteger 1, ASTNodeInteger 2],
    "test one integer: " ~: strToAST "(1)" ~?= ASTNodeArray [ASTNodeInteger 1]]


testIncRegisterImpl :: Bool
testIncRegisterImpl =
    regGet context EAX == Just 1
    where
        context = regInc (Just newContext) EAX

testIncRegister :: Test
testIncRegister = TestCase (assertBool "inc register" testIncRegisterImpl)

testIncRegisterInvalidContextImpl :: Bool
testIncRegisterInvalidContextImpl =
    case regInc Nothing EAX of
        Nothing -> True
        _ -> False

testIncRegisterInvalidContext :: Test
testIncRegisterInvalidContext = TestCase (assertBool "inc register invalid context" testIncRegisterInvalidContextImpl)

testDecRegisterImpl :: Bool
testDecRegisterImpl =
    regGet context EAX == Just (-1)
    where
        context = regDec (Just newContext) EAX

testDecRegister :: Test
testDecRegister = TestCase (assertBool "dec register" testDecRegisterImpl)

testDecRegisterInvalidContextImpl :: Bool
testDecRegisterInvalidContextImpl =
    case regDec Nothing EAX of
        Nothing -> True
        _ -> False

testDecRegisterInvalidContext :: Test
testDecRegisterInvalidContext = TestCase (assertBool "dec register invalid context" testDecRegisterInvalidContextImpl)

testAddRegisterImpl :: Bool
testAddRegisterImpl =
    regGet context EAX == Just 3
    where
        context = regAdd (Just newContext) EAX 3

testAddRegister :: Test
testAddRegister = TestCase (assertBool "add register" testAddRegisterImpl)

testAddRegisterInvalidContextImpl :: Bool
testAddRegisterInvalidContextImpl =
    case regAdd Nothing EAX 3 of
        Nothing -> True
        _ -> False

testAddRegisterInvalidContext :: Test
testAddRegisterInvalidContext = TestCase (assertBool "add register invalid context" testAddRegisterInvalidContextImpl)

testSubRegisterImpl :: Bool
testSubRegisterImpl =
    regGet context EAX == Just (-3)
    where
        context = regSub (Just newContext) EAX 3

testSubRegister :: Test
testSubRegister = TestCase (assertBool "sub register" testSubRegisterImpl)

testSubRegisterInvalidContextImpl :: Bool
testSubRegisterInvalidContextImpl =
    case regSub Nothing EAX 3 of
        Nothing -> True
        _ -> False

testSubRegisterInvalidContext :: Test
testSubRegisterInvalidContext = TestCase (assertBool "sub register invalid context" testSubRegisterInvalidContextImpl)

testMulRegisterImpl :: Bool
testMulRegisterImpl =
    regGet context EAX == Just 9
    where
        context = regMul (regSet (Just newContext) EAX 3) EAX 3

testMulRegister :: Test
testMulRegister = TestCase (assertBool "mul register" testMulRegisterImpl)

testMulRegisterInvalidContextImpl :: Bool
testMulRegisterInvalidContextImpl =
    case regMul Nothing EAX 3 of
        Nothing -> True
        _ -> False

testMulRegisterInvalidContext :: Test
testMulRegisterInvalidContext = TestCase (assertBool "mul register invalid context" testMulRegisterInvalidContextImpl)

testDivRegisterImpl :: Bool
testDivRegisterImpl =
    regGet context EAX == Just 3
    where
        context = regDiv (regSet (Just newContext) EAX 9) EAX 3

testDivRegister :: Test
testDivRegister = TestCase (assertBool "div register" testDivRegisterImpl)

testDivZeroRegisterImpl :: Bool
testDivZeroRegisterImpl =
    case regDiv (regSet (Just newContext) EAX 9) EAX 0 of
        Nothing -> True
        _ -> False

testDivZeroRegister :: Test
testDivZeroRegister = TestCase (assertBool "div zero register" testDivZeroRegisterImpl)

testDivRegisterInvalidContextImpl :: Bool
testDivRegisterInvalidContextImpl =
    case regDiv Nothing EAX 3 of
        Nothing -> True
        _ -> False

testDivRegisterInvalidContext :: Test
testDivRegisterInvalidContext = TestCase (assertBool "div register invalid context" testDivRegisterInvalidContextImpl)

testModRegisterImpl :: Bool
testModRegisterImpl =
    regGet context EAX == Just 1
    where
        context = regMod (regSet (Just newContext) EAX 9) EAX 2

testModRegister :: Test
testModRegister = TestCase (assertBool "mod register" testModRegisterImpl)

testModZeroRegisterImpl :: Bool
testModZeroRegisterImpl =
    case regMod (regSet (Just newContext) EAX 9) EAX 0 of
        Nothing -> True
        _ -> False

testModZeroRegister :: Test
testModZeroRegister = TestCase (assertBool "mod zero register" testModZeroRegisterImpl)

testModRegisterInvalidContextImpl :: Bool
testModRegisterInvalidContextImpl =
    case regMod Nothing EAX 3 of
        Nothing -> True
        _ -> False

testModRegisterInvalidContext :: Test
testModRegisterInvalidContext = TestCase (assertBool "mod register invalid context" testModRegisterInvalidContextImpl)

testAndRegisterImpl :: Bool
testAndRegisterImpl =
    regGet context EAX == Just 1
    where
        context = regAnd (regSet (Just newContext) EAX 5) EAX 3

testAndRegister :: Test
testAndRegister = TestCase (assertBool "and register" testAndRegisterImpl)

testAndRegisterInvalidContextImpl :: Bool
testAndRegisterInvalidContextImpl =
    case regAnd Nothing EAX 3 of
        Nothing -> True
        _ -> False

testAndRegisterInvalidContext :: Test
testAndRegisterInvalidContext = TestCase (assertBool "and register invalid context" testAndRegisterInvalidContextImpl)

testOrRegisterImpl :: Bool
testOrRegisterImpl =
    regGet context EAX == Just 7
    where
        context = regOr (regSet (Just newContext) EAX 5) EAX 3

testOrRegister :: Test
testOrRegister = TestCase (assertBool "or register" testOrRegisterImpl)

testOrRegisterInvalidContextImpl :: Bool
testOrRegisterInvalidContextImpl =
    case regOr Nothing EAX 3 of
        Nothing -> True
        _ -> False

testOrRegisterInvalidContext :: Test
testOrRegisterInvalidContext = TestCase (assertBool "or register invalid context" testOrRegisterInvalidContextImpl)

testXorRegisterImpl :: Bool
testXorRegisterImpl =
    regGet context EAX == Just 6
    where
        context = regXor (regSet (Just newContext) EAX 5) EAX 3

testXorRegister :: Test
testXorRegister = TestCase (assertBool "xor register" testXorRegisterImpl)

testXorRegisterInvalidContextImpl :: Bool
testXorRegisterInvalidContextImpl =
    case regXor Nothing EAX 3 of
        Nothing -> True
        _ -> False

testXorRegisterInvalidContext :: Test
testXorRegisterInvalidContext = TestCase (assertBool "xor register invalid context" testXorRegisterInvalidContextImpl)

testNotRegisterImpl :: Bool
testNotRegisterImpl =
    regGet context EAX == Just (-2)
    where
        context = regNot (regSet (Just newContext) EAX 1) EAX

testNotRegister :: Test
testNotRegister = TestCase (assertBool "not register" testNotRegisterImpl)

testNotRegisterInvalidContextImpl :: Bool
testNotRegisterInvalidContextImpl =
    case regNot Nothing EAX of
        Nothing -> True
        _ -> False

testNotRegisterInvalidContext :: Test
testNotRegisterInvalidContext = TestCase (assertBool "not register invalid context" testNotRegisterInvalidContextImpl)

testStackPushPopImpl :: Bool
testStackPushPopImpl =
    value == 3
    where
        (v, _) = case stackPop (stackPush (stackPush (Just newContext) 2) 3) of
            Nothing -> (Nothing, Nothing)
            Just (one, Just two) -> (Just one, Just two)
            Just (_, Nothing) -> (Just 987654321, Nothing)
        value = Data.fromMaybe 987654321 v

testStackPushPop :: Test
testStackPushPop = TestCase (assertBool "stack push pop peek" testStackPushPopImpl)

testStackPushPopPeekImpl :: Bool
testStackPushPopPeekImpl =
    Data.fromMaybe 0 value == 2
    where -- caution, the code executes from the bottom to the top
        (_, c) = case stackPop (stackPush (stackPush (Just newContext) 2) 3) of
            Nothing -> (Nothing, Nothing)
            Just (one, Just two) -> (Just one, Just two)
            Just (_, Nothing) -> (Just 987654321, Nothing)
        (value, _) = case stackPeek c of
            Nothing -> (Nothing, Nothing)
            Just (one, Just two) -> (Just one, Just two)
            Just (_, Nothing) -> (Just 987654321, Nothing)

testStackPushPopPeek :: Test
testStackPushPopPeek = TestCase (assertBool "stack push pop peek" testStackPushPopPeekImpl)

testStackDup :: Test
testStackDup = TestCase (assertBool "stack dup" testStackDupImpl)

testStackDupImpl :: Bool
testStackDupImpl =
    Data.fromMaybe 0 value == 2
    where -- caution, the code executes from the bottom to the top
        (_, c) = case stackPop (stackPush (stackPush (Just newContext) 2) 3) of
            Nothing -> (Nothing, Nothing)
            Just (one, Just two) -> (Just one, Just two)
            Just (_, Nothing) -> (Just 987654321, Nothing)
        (value, _) = case stackPeek (stackDup c) of
            Nothing -> (Nothing, Nothing)
            Just (one, Just two) -> (Just one, Just two)
            Just (_, Nothing) -> (Just 987654321, Nothing)

testStackSwap :: Test
testStackSwap = TestCase (assertBool "stack swap" testStackSwapImpl)

testStackSwapImpl :: Bool
testStackSwapImpl =
    Data.fromMaybe 0 value == 2
    where -- caution, the code executes from the bottom to the top
        c = stackPush (stackPush (Just newContext) 2) 3
        (value, _) = case stackPeek (stackSwap c) of
            Nothing -> (Nothing, Nothing)
            Just (one, Just two) -> (Just one, Just two)
            Just (_, Nothing) -> (Just 987654321, Nothing)

testStackRot :: Test
testStackRot = TestCase (assertBool "stack rot" testStackRotImpl)

testStackRotImpl :: Bool
testStackRotImpl =
    Data.fromMaybe 0 value == 2
    where -- caution, the code executes from the bottom to the top
        c = stackPush (stackPush (stackPush (Just newContext) 2) 3) 4
        (value, _) = case stackPeek (stackRot c) of
            Nothing -> (Nothing, Nothing)
            Just (one, Just two) -> (Just one, Just two)
            Just (_, Nothing) -> (Just 987654321, Nothing)

testHeapAlloc :: Test
testHeapAlloc = TestCase (assertBool "heap set get" testHeapAllocImpl)

testHeapAllocImpl :: Bool
testHeapAllocImpl =
    addr == Just 2
    where -- caution, the code executes from the bottom to the top
        (addr, _) = case heapAlloc c of
            Nothing -> (Nothing, Nothing)
            Just (one, Just two) -> (Just one, Just two)
            Just (_, Nothing) -> (Just 987654321, Nothing)
        (_, c) = case heapAlloc (Just newContext) of
            Nothing -> (Nothing, Nothing)
            Just (one, Just two) -> (Just one, Just two)
            Just (_, Nothing) -> (Just 987654321, Nothing)

testHeapAllocBisImpl :: Int
testHeapAllocBisImpl =
    addr
    where -- caution, the code executes from the bottom to the top
        (addr, _) = case heapAlloc (Just newContext) of
            Nothing -> (-1, Nothing)
            Just (one, Just two) -> (one, Just two)
            Just (_, Nothing) -> (-1, Nothing)

testHeapAllocBis :: Test
testHeapAllocBis = TestCase (assertEqual "heap alloc bis" 1 testHeapAllocBisImpl)


testHeapSetGet :: Test
testHeapSetGet = TestCase (assertBool "heap set get" testHeapSetGetImpl)

testHeapSetGetImpl :: Bool
testHeapSetGetImpl =
    value == 42
    where
        value = Data.fromMaybe 98764321 (heapGet (heapSet ctx 1 42) 1)
        ctx = case heapAlloc (Just newContext) of
            Nothing -> Nothing
            Just (_, Just two) -> Just two
            Just (_, Nothing) -> Nothing

testLabelSetGetImpl :: Bool
testLabelSetGetImpl =
    value == Just 42
    where
        value = labelGet c "ouioui"
        c = labelSet (Just newContext) "ouioui" 42

testLabelSetGet :: Test
testLabelSetGet = TestCase (assertBool "label set get" testLabelSetGetImpl)

testFlagGetSetImpl :: Bool
testFlagGetSetImpl =
    value == True
    where
        value = flagGet c ZF
        c = flagSet (Just newContext) ZF True

testFlagGetSet :: Test
testFlagGetSet = TestCase (assertBool "flag get set" testFlagGetSetImpl)

testMovImpl :: Bool
testMovImpl =
    regGet context2 EBX == Just 42
    where
        -- context2 = regSet (Just newContext)
        context2 = instructionTable context (Mov (Reg EBX) (Reg EAX))
        context = instructionTable (Just newContext) (Mov (Reg EAX) (Immediate 42))

testMov :: Test
testMov = TestCase (assertBool "mov" testMovImpl)

testAddImpl :: Bool
testAddImpl =
    regGet context3 EBX == Just 43
    where
        -- context2 = regSet (Just newContext)
        context3 = instructionTable context2 ( (Add EBX (Reg EAX)))
        context2 = instructionTable context1 ( (Add EBX (Immediate 1)))
        context1 = instructionTable context ( (Mov (Reg EBX) (Immediate 0)))
        context = instructionTable (Just newContext) ( (Mov (Reg EAX) (Immediate 42)))

testAdd :: Test
testAdd = TestCase (assertBool "add" testAddImpl)

testCmpImpl1 :: Bool
testCmpImpl1 =
    if (flagGet c ZF == True) then True else False
    where
        c = instructionTable context1 ( (Cmp (Reg EBX) (Reg EAX)))
        context1 = instructionTable context ( (Mov (Reg EBX) (Immediate 42)))
        context = instructionTable (Just newContext) ( (Mov (Reg EAX) (Immediate 42)))

testCmpImpl2 :: Bool
testCmpImpl2 =
    if (flagGet c ZF == True) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 42)))
        context = instructionTable (Just newContext) ( (Mov (Reg EAX) (Immediate 42)))

testCmpImpl3 :: Bool
testCmpImpl3 =
    if (flagGet c ZF == False) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 43)))
        context = instructionTable (Just newContext) ( (Mov (Reg EAX) (Immediate 42)))

testCmpImpl4 :: Bool
testCmpImpl4 =
    if (flagGet c SF == True) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 43)))
        context = instructionTable (Just newContext) ( (Mov (Reg EAX) (Immediate 42)))

testCmpImpl5 :: Bool
testCmpImpl5 =
    if (flagGet c SF == False) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 41)))
        context = instructionTable (Just newContext) ( (Mov (Reg EAX) (Immediate 42)))

testCmpImpl6 :: Bool
testCmpImpl6 =
    if (flagGet c OF == False) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 410)))
        context = instructionTable (Just newContext) (Mov (Reg EAX) (Immediate (-42)))

testCmpImpl7 :: Bool
testCmpImpl7 =
    if (flagGet c OF == False) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 43)))
        context = instructionTable (Just newContext) (Mov (Reg EAX) (Immediate (-42)))

testCmpImpl9 :: Bool
testCmpImpl9 =
    if (flagGet c CF == True) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 43)))
        context = instructionTable (Just newContext) (Mov (Reg EAX) (Immediate 42))

testCmpImpl8 :: Bool
testCmpImpl8 =
    if (flagGet c CF == False) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 41)))
        context = instructionTable (Just newContext) (Mov (Reg EAX) (Immediate 42))


testCmp :: Test
testCmp = TestList [
    "Cmp Carry 1 reg 1 Im" ~: testCmpImpl9 ~?= True,
    "Cmp Carry 1 reg 1 Im" ~: testCmpImpl8 ~?= True,
    "Cmp Overflow 1 reg 1 Im" ~: testCmpImpl7 ~?= True,
    "Cmp Overflow 1 reg 1 Im" ~: testCmpImpl6 ~?= True, -- overflow c'est chiant a tester donc tkt ca marche
    "Cmp negative 1 reg 1 Im" ~: testCmpImpl5 ~?= True,
    "Cmp positive 1 reg 1 Im" ~: testCmpImpl4 ~?= True,
    "Cmp not eq 1 reg 1 Im" ~: testCmpImpl3 ~?= True,
    "Cmp eq 1 reg 1 Im" ~: testCmpImpl2 ~?= True,
    "Cmp eq 2 reg" ~: testCmpImpl1 ~?= True]

testIncImpl :: Bool
testIncImpl =
    regGet context1 EBX == Just 43
    where
        context1 = instructionTable context (Inc EBX)
        context = instructionTable (Just newContext) ( (Mov (Reg EBX) (Immediate 42)))

testDecImpl :: Bool
testDecImpl =
    regGet context1 EBX == Just 41
    where
        context1 = instructionTable context (Dec EBX)
        context = instructionTable (Just newContext) ( (Mov (Reg EBX) (Immediate 42)))

testNegImpl :: Bool
testNegImpl =
    regGet context1 EBX == Just (-42)
    where
        context1 = instructionTable context (Neg EBX)
        context = instructionTable (Just newContext) ( (Mov (Reg EBX) (Immediate 42)))

testInc :: Test
testInc = TestList [
    "Dec 1 reg" ~: testDecImpl ~?= True,
    "Neg 1 reg" ~: testNegImpl ~?= True,
    "Inc 1 reg" ~: testIncImpl ~?= True]


main :: IO ()
main = do
    _ <- runTestTT testTryTokenizeOne
    _ <- runTestTT testWordToToken
    _ <- runTestTT testTokenize
    _ <- runTestTT testTokOrExprToNode
    _ <- runTestTT testTryToMatch
    _ <- runTestTT testBuildASTIterate
    _ <- runTestTT testBuildAST
    _ <- runTestTT testStrToAST
    _ <- runTestTT testIncRegister
    _ <- runTestTT testIncRegisterInvalidContext
    _ <- runTestTT testDecRegister
    _ <- runTestTT testDecRegisterInvalidContext
    _ <- runTestTT testAddRegister
    _ <- runTestTT testAddRegisterInvalidContext
    _ <- runTestTT testSubRegister
    _ <- runTestTT testSubRegisterInvalidContext
    _ <- runTestTT testMulRegister
    _ <- runTestTT testMulRegisterInvalidContext
    _ <- runTestTT testDivRegister
    _ <- runTestTT testDivZeroRegister
    _ <- runTestTT testDivRegisterInvalidContext
    _ <- runTestTT testModRegister
    _ <- runTestTT testModZeroRegister
    _ <- runTestTT testModRegisterInvalidContext
    _ <- runTestTT testAndRegister
    _ <- runTestTT testAndRegisterInvalidContext
    _ <- runTestTT testOrRegister
    _ <- runTestTT testOrRegisterInvalidContext
    _ <- runTestTT testXorRegister
    _ <- runTestTT testXorRegisterInvalidContext
    _ <- runTestTT testNotRegister
    _ <- runTestTT testNotRegisterInvalidContext
    _ <- runTestTT testStackPushPopPeek
    _ <- runTestTT testStackPushPop
    _ <- runTestTT testStackDup
    _ <- runTestTT testStackSwap
    _ <- runTestTT testStackRot
    _ <- runTestTT testHeapAlloc
    _ <- runTestTT testHeapAllocBis
    _ <- runTestTT testHeapSetGet
    _ <- runTestTT testLabelSetGet
    _ <- runTestTT testFlagGetSet
    _ <- runTestTT testMov
    _ <- runTestTT testAdd
    _ <- runTestTT testCmp
    _ <- runTestTT testInc
    _ <- runTestTT testASTNodeParamList
    _ <- runTestTT testASTNodeArray
    return ()

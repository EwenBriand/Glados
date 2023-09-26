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
              ASTNodeSymbol, astniValue),
      TokorNode(T, A) )
import VM( regGet, regSet, regInc, newContext, Register(EAX), regDec, regAdd,
           regSub, regMul, regDiv, regMod,
           regAnd, regOr, regXor, regNot,
           newStack, stackPush, stackPop, stackPeek, stackDup, stackSwap,
           stackRot, newHeap, heapSet, heapGet, heapAlloc, heapFree,
           newLabels, labelSet, labelGet, labelFree,
           newFlags, flagSet, flagGet, Flag(ZF))
import qualified Data.Maybe as Data


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
    value == Just True
    where
        value = flagGet c ZF
        c = flagSet (Just newContext) ZF True

testFlagGetSet :: Test
testFlagGetSet = TestCase (assertBool "flag get set" testFlagGetSetImpl)

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
    return ()

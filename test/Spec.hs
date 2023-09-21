import Test.HUnit
import Tokenizer
import Lexer

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
    return ()

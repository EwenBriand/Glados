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

testLexertokWordtoExpr :: Test
testLexertokWordtoExpr = TestList [
    "tokWordToExpr empty string" ~: tokWordToExpr [] ~?= ExprEmpty,
    "tokWordToExpr Symbol" ~: tokWordToExpr [T (TokenInfo TokSymbol "abc")] ~?= ExprSymbol,
    "tokWordToExpr integer" ~: tokWordToExpr [T (TokenInfo TokInteger "123")] ~?= ExprInteger,
    "tokWordToExpr plus" ~: tokWordToExpr [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorPlus "+"), E ExprInteger, E ExprInteger, T (TokenInfo TokCloseParen ")")] ~?= ExprSum,
    "tokWordToExpr minus" ~: tokWordToExpr [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorMinus "-"), E ExprInteger, E ExprInteger, T (TokenInfo TokCloseParen ")")] ~?= ExprSub,
    "tokWordToExpr div" ~: tokWordToExpr [T (TokenInfo TokOpenParen "("), T (TokenInfo TokOperatorDiv "/"), E ExprInteger, E ExprInteger, T (TokenInfo TokCloseParen ")")] ~?= ExprDiv]



main :: IO ()
main = do
    _ <- runTestTT testTryTokenizeOne
    _ <- runTestTT testWordToToken
    _ <- runTestTT testTokenize
    _ <- runTestTT testLexertokWordtoExpr
    return ()

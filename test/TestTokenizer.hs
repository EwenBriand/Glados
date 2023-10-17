module TestTokenizer
  ( testWordToToken,
    testTryTokenizeOne,
    testTokenInfoFields,
    testTokenInfoShow,
    testTokenize,
  )
where

import Test.HUnit
import Tokenizer

testWordToToken :: Test
testWordToToken =
  TestList
    [ "WordToToken empty string" ~: wordToTok "" ~?= TokenInfo TokEmpty "",
      "WordToToken Symbol" ~: wordToTok "abc" ~?= TokenInfo TokSymbol "abc",
      "WordToToken integer" ~: wordToTok "123" ~?= TokenInfo TokInteger "123",
      "WordToToken plus 1" ~: wordToTok "+" ~?= TokenInfo TokOperatorPlus "+",
      "WordToToken plus 2" ~: wordToTok "add" ~?= TokenInfo TokOperatorPlus "add",
      "WordToToken minus 1" ~: wordToTok "-" ~?= TokenInfo TokOperatorMinus "-",
      "WordToToken minus 2" ~: wordToTok "sub" ~?= TokenInfo TokOperatorMinus "sub",
      "WordToToken mul 1" ~: wordToTok "*" ~?= TokenInfo TokOperatorMul "*",
      "WordToToken mul 2" ~: wordToTok "mul" ~?= TokenInfo TokOperatorMul "mul",
      "WordToToken div 1" ~: wordToTok "/" ~?= TokenInfo TokOperatorDiv "/",
      "WordToToken div 2" ~: wordToTok "div" ~?= TokenInfo TokOperatorDiv "div",
      "WordToToken mod 1" ~: wordToTok "%" ~?= TokenInfo TokOperatorMod "%",
      "WordToToken mod 2" ~: wordToTok "mod" ~?= TokenInfo TokOperatorMod "mod",
      "WordToToken true" ~: wordToTok "true" ~?= TokenInfo TokenBool "true",
      "WordToToken #t" ~: wordToTok "#t" ~?= TokenInfo TokenBool "true",
      "WordToToken false" ~: wordToTok "false" ~?= TokenInfo TokenBool "false",
      "WordToToken #f" ~: wordToTok "#f" ~?= TokenInfo TokenBool "false",
      "WordToToken define" ~: wordToTok "mutable" ~?= TokenInfo TokKeywordMutable "mutable",
      "WordToToken comment" ~: wordToTok "//" ~?= TokenInfo TokComment "//",
      "WordToToken open paren" ~: wordToTok "(" ~?= TokenInfo TokOpenParen "(",
      "WordToToken close paren" ~: wordToTok ")" ~?= TokenInfo TokCloseParen ")",
      "WordToToken open bracket" ~: wordToTok "[" ~?= TokenInfo TokOpenBrac "[",
      "WordToToken close bracket" ~: wordToTok "]" ~?= TokenInfo TokCloseBrac "]",
      "WordToToken open curly bracket" ~: wordToTok "{" ~?= TokenInfo TokOpenCurrBrac "{",
      "WordToToken close curly bracket" ~: wordToTok "}" ~?= TokenInfo TokCloseCurrBrac "}",
      "WordToToken whitespace" ~: wordToTok " " ~?= TokenInfo TokWhitespace " ",
      "WordToToken newline" ~: wordToTok "\n" ~?= TokenInfo TokNewLine "\n",
      "WordToToken error" ~: wordToTok "°" ~?= TokenInfo TokError "°",
      "WordToToken then" ~: wordToTok "then" ~?= TokenInfo TokenKeywordThen "then",
      "WordToToken else" ~: wordToTok "else" ~?= TokenInfo TokenKeywordElse "else",
      "WordToToken elif" ~: wordToTok "elif" ~?= TokenInfo TokenElif "elif",
      "WordToToken eq?" ~: wordToTok "eq?" ~?= TokenInfo TokenEqual "eq?",
      "WordToToken <" ~: wordToTok "<" ~?= TokenInfo TokenInferior "<",
      "WordToToken <=" ~: wordToTok "<=" ~?= TokenInfo TokenInferiorEq "<=",
      "WordToToken >" ~: wordToTok ">" ~?= TokenInfo TokenSuperior ">",
      "WordToToken >=" ~: wordToTok ">=" ~?= TokenInfo TokenSuperiorEq ">=",
      "WordToToken !=" ~: wordToTok "!=" ~?= TokenInfo TokenNotEqual "!=",
      "WordToToken #" ~: wordToTok "#" ~?= TokenInfo TokenKeywordPartialExpression "#",
      "WordToToken !" ~: wordToTok "!" ~?= TokenInfo TokenKeywordPartialExpression "!",
      "WordToToken lambda" ~: wordToTok "lambda" ~?= TokenInfo TokLambda "lambda",
      "WordToToken print" ~: wordToTok "print" ~?= TokenInfo TokenSymPrint "print",
      "WordToToken int" ~: wordToTok "int" ~?= TokenInfo TokenType "int",
      "WordToToken bool" ~: wordToTok "bool" ~?= TokenInfo TokenType "bool",
      "WordToToken =" ~: wordToTok "=" ~?= TokenInfo TokenEq "=",
      "WordToToken :" ~: wordToTok ";" ~?= TokenInfo TokenPointComma ";",
      "WordToToken while" ~: wordToTok "while" ~?= TokenInfo TokenKeywordWhile "while",
      "WordToToken for" ~: wordToTok "for" ~?= TokenInfo TokenKeywordFor "for"
    ]

testTryTokenizeOne :: Test
testTryTokenizeOne =
  TestList
    [ "TryTokenizeOne empty string" ~: tryTokenizeOne "" (TokenInfo TokError "") "" ~?= (TokenInfo TokEmpty "", []),
      "TryTokenizeOne Symbol" ~: tryTokenizeOne "" (TokenInfo TokError "") "abc" ~?= (TokenInfo TokSymbol "abc", ""),
      "TryTokenizeOne integer" ~: tryTokenizeOne "" (TokenInfo TokError "") "123" ~?= (TokenInfo TokInteger "123", ""),
      "TryTokenizeOne plus" ~: tryTokenizeOne "" (TokenInfo TokError "") "+" ~?= (TokenInfo TokOperatorPlus "+", ""),
      "TryTokenizeOne mutable" ~: tryTokenizeOne "" (TokenInfo TokError "") "mutable" ~?= (TokenInfo TokKeywordMutable "mutable", ""),
      "TryTokenizeOne comment" ~: tryTokenizeOne "" (TokenInfo TokError "") "//" ~?= (TokenInfo TokComment "//", ""),
      "TrytokenizeOne div" ~: tryTokenizeOne "" (TokenInfo TokError "") "/" ~?= (TokenInfo TokOperatorDiv "/", ""),
      "TryTokenizeOne open paren" ~: tryTokenizeOne "" (TokenInfo TokError "") "(" ~?= (TokenInfo TokOpenParen "(", ""),
      "TryTokenizeOne close paren" ~: tryTokenizeOne "" (TokenInfo TokError "") ")" ~?= (TokenInfo TokCloseParen ")", ""),
      "TryTokenizeOne minus" ~: tryTokenizeOne "" (TokenInfo TokError "") "-" ~?= (TokenInfo TokOperatorMinus "-", ""),
      "TryTokenizeOne error" ~: tryTokenizeOne "" (TokenInfo TokError "") "°" ~?= (TokenInfo TokError "", "°"),
      "TryTokenizeOne true" ~: tryTokenizeOne "" (TokenInfo TokError "") "true" ~?= (TokenInfo TokenBool "true", ""),
      "TryTokenizeOne #t" ~: tryTokenizeOne "" (TokenInfo TokError "") "#t" ~?= (TokenInfo TokenBool "true", "")
    ]

testTokenInfoFields :: Test
testTokenInfoFields =
  test
    [ "Test TokenInfo fields"
        ~: let ti = TokenInfo {token = TokInteger, value = "123"}
            in do
                 assertEqual "Token should be TokInteger" TokInteger (token ti)
                 assertEqual "Value should be 'example'" "123" (value ti)
    ]

testTokenInfoShow :: Test
testTokenInfoShow =
  test
    [ "Test TokenInfo Show instance"
        ~: let ti = TokenInfo {token = TokInteger, value = "123"}
            in do
                 assertEqual "Show instance should match" "123" (show ti)
    ]

testTokenize :: Test
testTokenize =
  TestList
    [ "Tokenize empty string" ~: tokenize "" ~?= [],
      "Tokenize Symbol" ~: tokenize "abc" ~?= [TokenInfo TokSymbol "abc"],
      "Tokenize variable definition" ~: tokenize "mutable oui 123" ~?= [TokenInfo TokKeywordMutable "mutable", TokenInfo TokSymbol "oui", TokenInfo TokInteger "123"],
      "Tokenize Error" ~: tokenize "°" ~?= [TokenInfo TokError ""],
      "converts a Token to a string"
        ~: let x = TokInteger
            in show x ~?= "TokInteger"
    ]

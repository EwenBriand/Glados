module TestTokenizer (
    testWordToToken,
    testTryTokenizeOne,
    testTokenInfoFields,
    testTokenInfoShow,
    testTokenEnum,
    testTokenize
) where

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
      "WordToToken define" ~: wordToTok "define" ~?= TokenInfo TokKeyworddefine "define",
      "WordToToken comment" ~: wordToTok "//" ~?= TokenInfo TokComment "//",
      "WordToToken open paren" ~: wordToTok "(" ~?= TokenInfo TokOpenParen "(",
      "WordToToken close paren" ~: wordToTok ")" ~?= TokenInfo TokCloseParen ")",
      "WordToToken whitespace" ~: wordToTok " " ~?= TokenInfo TokWhitespace " ",
      "WordToToken newline" ~: wordToTok "\n" ~?= TokenInfo TokNewLine "\n",
      "WordToToken error" ~: wordToTok "°" ~?= TokenInfo TokError "°"
    ]

testTryTokenizeOne :: Test
testTryTokenizeOne =
  TestList
    [ "TryTokenizeOne empty string" ~: tryTokenizeOne "" (TokenInfo TokError "") "" ~?= (TokenInfo TokEmpty "", []),
      "TryTokenizeOne Symbol" ~: tryTokenizeOne "" (TokenInfo TokError "") "abc" ~?= (TokenInfo TokSymbol "abc", ""),
      "TryTokenizeOne integer" ~: tryTokenizeOne "" (TokenInfo TokError "") "123" ~?= (TokenInfo TokInteger "123", ""),
      "TryTokenizeOne plus" ~: tryTokenizeOne "" (TokenInfo TokError "") "+" ~?= (TokenInfo TokOperatorPlus "+", ""),
      "TryTokenizeOne define" ~: tryTokenizeOne "" (TokenInfo TokError "") "define" ~?= (TokenInfo TokKeyworddefine "define", ""),
      "TryTokenizeOne comment" ~: tryTokenizeOne "" (TokenInfo TokError "") "//" ~?= (TokenInfo TokComment "//", ""),
      "TrytokenizeOne div" ~: tryTokenizeOne "" (TokenInfo TokError "") "/" ~?= (TokenInfo TokOperatorDiv "/", ""),
      "TryTokenizeOne open paren" ~: tryTokenizeOne "" (TokenInfo TokError "") "(" ~?= (TokenInfo TokOpenParen "(", ""),
      "TryTokenizeOne close paren" ~: tryTokenizeOne "" (TokenInfo TokError "") ")" ~?= (TokenInfo TokCloseParen ")", ""),
      "TryTokenizeOne minus" ~: tryTokenizeOne "" (TokenInfo TokError "") "-" ~?= (TokenInfo TokOperatorMinus "-", ""),
      "TryTokenizeOne error" ~: tryTokenizeOne "" (TokenInfo TokError "") "°" ~?= (TokenInfo TokError "", "°")
    ]

testTokenInfoFields :: Test
testTokenInfoFields = test
  [ "Test TokenInfo fields" ~:
    let ti = TokenInfo { token = TokInteger, value = "123" }
    in do
      assertEqual "Token should be TokInteger" TokInteger (token ti)
      assertEqual "Value should be 'example'" "123" (value ti)
  ]

testTokenInfoShow :: Test
testTokenInfoShow = test
  [ "Test TokenInfo Show instance" ~:
    let ti = TokenInfo { token = TokInteger, value = "123" }
    in do
      assertEqual "Show instance should match" "TokenInfo {token = TokInteger, value = \"123\"}" (show ti)
  ]

testTokenEnum :: Test
testTokenEnum = test
  [ "Test Token Enum instance" ~:
    let
      expectedInt = 4
      expectedToken = TokOperatorMul
    in do
      assertEqual "Enum conversion to Int" expectedInt (fromEnum expectedToken)
      assertEqual "Enum conversion from Int" expectedToken (toEnum expectedInt)
  ]

testTokenize :: Test
testTokenize =
  TestList
    [ "Tokenize empty string" ~: tokenize "" ~?= [],
      "Tokenize Symbol" ~: tokenize "abc" ~?= [TokenInfo TokSymbol "abc"],
      "Tokenize variable definition" ~: tokenize "define oui 123" ~?= [TokenInfo TokKeyworddefine "define", TokenInfo TokSymbol "oui", TokenInfo TokInteger "123"],
      "Tokenize Error" ~: tokenize "°" ~?= [TokenInfo TokError ""]
    ]

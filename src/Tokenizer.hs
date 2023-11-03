{-# LANGUAGE DeriveGeneric #-}

module Tokenizer
  ( Token (..),
    tokenize,
    wordToTok,
    tryTokenizeOne,
    TokenInfo (..),
  )
where

import Data.Char (isAlpha, isDigit)
import GHC.Generics (Generic)
import Data.Binary

data Token
  = TokSymbol
  | TokInteger
  | TokOperatorPlus 
  | TokOperatorMinus 
  | TokOperatorMul 
  | TokOperatorDiv 
  | TokOperatorMod 
  | TokKeywordMutable 
  | TokKeywordDefine 
  | TokLambda 
  | TokComment 
  | TokOpenParen 
  | TokCloseParen 
  | TokError
  | TokEmpty
  | TokWhitespace
  | TokNewLine
  | TokenBool
  | TokenKeywordIf
  | TokenKeywordThen
  | TokenKeywordElse
  | TokenKeywordPartialExpression
  | TokenEqual
  | TokenInferior
  | TokenInferiorEq
  | TokenSuperior
  | TokenSuperiorEq
  | TokenNotEqual
  | TokenSymPrint 
  | TokOpenBrac 
  | TokCloseBrac 
  | TokOpenCurrBrac 
  | TokCloseCurrBrac 
  | TokenEq 
  | TokenPointComma 
  | TokenComma 
  | TokenElif 
  | TokenKeywordWhile 
  | TokenKeywordFor 
  | TokenType 
  | TokenReturn 
  | TokenDeref 
  | TokenCast 
  | TokenInclude 
  | TokenShowKeyword
  | TokenAsKeyword 
  | TokenPoint
  | TokenKeywordStruct

  deriving (Eq, Show, Generic)

instance Binary Token

data TokenInfo = TokenInfo {token :: Token, value :: String} deriving (Eq, Generic)

instance Binary TokenInfo

instance Show TokenInfo where
  show (TokenInfo _ v) = v

wordToTok :: String -> TokenInfo
wordToTok "" = TokenInfo {token = TokEmpty, value = ""}
wordToTok "mutable" = TokenInfo {token = TokKeywordMutable, value = "mutable"}
wordToTok "define" = TokenInfo {token = TokKeywordDefine, value = "define"} 
wordToTok "+" = TokenInfo {token = TokOperatorPlus, value = "+"}
wordToTok "add" = TokenInfo {token = TokOperatorPlus, value = "add"}
wordToTok "-" = TokenInfo {token = TokOperatorMinus, value = "-"}
wordToTok "sub" = TokenInfo {token = TokOperatorMinus, value = "sub"}
wordToTok "*" = TokenInfo {token = TokOperatorMul, value = "*"}
wordToTok "mul" = TokenInfo {token = TokOperatorMul, value = "mul"}
wordToTok "/" = TokenInfo {token = TokOperatorDiv, value = "/"}
wordToTok "div" = TokenInfo {token = TokOperatorDiv, value = "div"}
wordToTok "%" = TokenInfo {token = TokOperatorMod, value = "%"}
wordToTok "mod" = TokenInfo {token = TokOperatorMod, value = "mod"}
wordToTok "//" = TokenInfo {token = TokComment, value = "//"}
wordToTok "(" = TokenInfo {token = TokOpenParen, value = "("}
wordToTok ")" = TokenInfo {token = TokCloseParen, value = ")"}
wordToTok "[" = TokenInfo {token = TokOpenBrac, value = "["}
wordToTok "]" = TokenInfo {token = TokCloseBrac, value = "]"}
wordToTok "{" = TokenInfo {token = TokOpenCurrBrac, value = "{"}
wordToTok "}" = TokenInfo {token = TokCloseCurrBrac, value = "}"}
wordToTok " " = TokenInfo {token = TokWhitespace, value = " "}
wordToTok "\n" = TokenInfo {token = TokNewLine, value = "\n"}
wordToTok "#t" = TokenInfo {token = TokenBool, value = "true"}
wordToTok "true" = TokenInfo {token = TokenBool, value = "true"}
wordToTok "#f" = TokenInfo {token = TokenBool, value = "false"}
wordToTok "false" = TokenInfo {token = TokenBool, value = "false"}
wordToTok "if" = TokenInfo {token = TokenKeywordIf, value = "if"}
wordToTok "then" = TokenInfo {token = TokenKeywordThen, value = "then"}
wordToTok "else" = TokenInfo {token = TokenKeywordElse, value = "else"}
wordToTok "elif" = TokenInfo {token = TokenElif, value = "elif"}
wordToTok "eq?" = TokenInfo {token = TokenEqual, value = "eq?"}
wordToTok "<" = TokenInfo {token = TokenInferior, value = "<"}
wordToTok "<=" = TokenInfo {token = TokenInferiorEq, value = "<="}
wordToTok ">" = TokenInfo {token = TokenSuperior, value = ">"}
wordToTok ">=" = TokenInfo {token = TokenSuperiorEq, value = ">="}
wordToTok "!=" = TokenInfo {token = TokenNotEqual, value = "!="}
wordToTok "#" = TokenInfo {token = TokenKeywordPartialExpression, value = "#"}
wordToTok "!" = TokenInfo {token = TokenKeywordPartialExpression, value = "!"}
wordToTok "lambda" = TokenInfo {token = TokLambda, value = "lambda"}
wordToTok "print" = TokenInfo {token = TokenSymPrint, value = "print"}
wordToTok "int" = TokenInfo {token = TokenType, value = "int"}
wordToTok "bool" = TokenInfo {token = TokenType, value = "bool"}
wordToTok "@" = TokenInfo {token = TokenType, value = "@"}
wordToTok "=" = TokenInfo {token = TokenEq, value = "="}
wordToTok ";" = TokenInfo {token = TokenPointComma, value = ";"}
wordToTok "," = TokenInfo {token = TokenComma, value = ","}
wordToTok "while" = TokenInfo {token = TokenKeywordWhile, value = "while"}
wordToTok "for" = TokenInfo {token = TokenKeywordFor, value = "for"}
wordToTok "return" = TokenInfo {token = TokenReturn, value = "return"}
wordToTok ":" = TokenInfo {token = TokenKeywordPartialExpression, value = ":"}
wordToTok "::" = TokenInfo {token = TokenDeref, value = "deref"}
wordToTok "as" = TokenInfo {token = TokenCast, value = "as"}
wordToTok "show" = TokenInfo {token = TokenShowKeyword, value = "show"}
wordToTok "as" = TokenInfo {token = TokenAsKeyword, value = "as"}
wordToTok "." = TokenInfo {token = TokenPoint, value = "."}
wordToTok "struct" = TokenInfo {token = TokenKeywordStruct, value = "struct"}
wordToTok str
  | all isAlpha str = TokenInfo {token = TokSymbol, value = str}
  | all isDigit str = TokenInfo {token = TokInteger, value = str}
  | otherwise = TokenInfo {token = TokError, value = str}

-- | @params:
--     currword: the current word for which we are trying to find a match.
--     lastmatch: the last valid match. First call the function with TokError;
--     xs: the rest of the string.
-- @return: a token, and the rest of the string that has not been consumed
tryTokenizeOne :: String -> TokenInfo -> String -> (TokenInfo, String)
tryTokenizeOne [] _ [] = (TokenInfo TokEmpty "", [])
tryTokenizeOne _ lastmatch [] = (lastmatch, [])
tryTokenizeOne currword lastmatch (x : xs) = case wordToTok (currword ++ [x]) of
  TokenInfo TokError _ -> (lastmatch, x : xs)
  anytok -> tryTokenizeOne (currword ++ [x]) anytok xs

-- @params:
--     str: the string to tokenize
-- @return: a list of tokens that represent the information contained in the string.
-- For example, "define x 123" would Prelude.return [TokKeywordMutable, TokSymbol, TokInteger]
-- Whitespaces are ignored.
tokenize :: String -> [TokenInfo]
tokenize [] = []
tokenize str
  | str == rest = [firstTok]
  | firstTok == TokenInfo TokWhitespace " " = tokenize rest
  | firstTok == TokenInfo TokNewLine "\n" = tokenize rest
  | otherwise = firstTok : tokenize rest
  where
    (firstTok, rest) = tryTokenizeOne "" (TokenInfo TokError "") str

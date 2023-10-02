module Tokenizer (
    Token(..),
    tokenize,
    wordToTok,
    tryTokenizeOne,
    TokenInfo(..)
) where

import Data.Char (isAlpha, isDigit)
-- | A token is a representation of a word in the language.
-- For example, "define" is a token, "123" is a token, etc.
-- When creating or deleting a token, do not forget to update the wordToTok function
-- to reflect those changes !!!
data Token = TokSymbol -- ^ A variable name, function name, etc.
           | TokInteger-- ^ Representation of an integer, e.g. "123"
           | TokOperatorPlus -- The sum operator, "+"
           | TokOperatorMinus -- The subtraction operator, "-"
           | TokOperatorMul -- The multiplication operator, "*"
           | TokOperatorDiv -- The division operator, "/"
           | TokOperatorMod -- The modulo operator, "%"
           | TokKeyworddefine -- The "define" keyword, used to define a symbol
           | TokComment -- Converts the rest of the line into a comment
           | TokOpenParen -- The open parenthesis character
           | TokCloseParen -- The close parenthesis character
           | TokError  -- ^ Any other character not mentioned above
           | TokEmpty -- ^ The empty token
           | TokWhitespace -- ^ A whitespace character
           | TokNewLine -- ^ A newline character
           | TokenBool -- ^ A boolean value
           | TokenKeywordIf -- ^ The "if" keyword
           deriving (Eq, Show, Enum)

data TokenInfo = TokenInfo { token :: Token, value :: String} deriving (Eq, Show)

-- | @params:
--     str: the string to tokenize
-- @return: a token that matches the string, or TokError
wordToTok :: String -> TokenInfo
wordToTok "" = TokenInfo {token = TokEmpty, value = ""}
wordToTok "define" = TokenInfo {token = TokKeyworddefine, value = "define"}
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
wordToTok " " = TokenInfo {token = TokWhitespace, value = " "}
wordToTok "\n" = TokenInfo {token = TokNewLine, value = "\n"}
wordToTok "\\n" = TokenInfo {token = TokNewLine, value = "\n"}
wordToTok "#t" = TokenInfo {token = TokenBool, value = "true"}
wordToTok "#f" = TokenInfo {token = TokenBool, value = "false"}
wordToTok "true" = TokenInfo {token = TokenBool, value = "true"}
wordToTok "false" = TokenInfo {token = TokenBool, value = "false"}
wordToTok "if" = TokenInfo {token = TokenKeywordIf, value = "if"}
wordToTok str | all isAlpha str = TokenInfo {token = TokSymbol, value = str}
                | all isDigit str = TokenInfo {token = TokInteger, value = str}
                | otherwise = TokenInfo { token = TokError, value = str}

-- | @params:
--     currword: the current word for which we are trying to find a match.
--     lastmatch: the last valid match. First call the function with TokError;
--     xs: the rest of the string.
-- @return: a token, and the rest of the string that has not been consumed
tryTokenizeOne :: String -> TokenInfo -> String -> (TokenInfo, String)
tryTokenizeOne [] _ [] = (TokenInfo TokEmpty "", [])
tryTokenizeOne _ lastmatch [] = (lastmatch, [])
-- tryTokenizeOne " " lastmatch (x:xs) = tryTokenizeOne [x] lastmatch xs
tryTokenizeOne currword lastmatch (x:xs) = case wordToTok (currword ++ [x]) of
    TokenInfo TokError _ -> (lastmatch, x:xs)
    anytok -> tryTokenizeOne (currword ++ [x]) anytok xs

-- @params:
--     str: the string to tokenize
-- @return: a list of tokens that represent the information contained in the string.
-- For example, "define x 123" would return [TokKeyworddefine, TokSymbol, TokInteger]
-- Whitespaces are ignored.
tokenize :: String -> [TokenInfo]
tokenize [] = []
tokenize str | str == rest = [firstTok]
             | firstTok == TokenInfo TokWhitespace " " = tokenize rest
             | firstTok == TokenInfo TokNewLine "\n" = tokenize rest
             | otherwise = firstTok : tokenize rest
            where
                (firstTok, rest) = tryTokenizeOne "" (TokenInfo TokError "") str

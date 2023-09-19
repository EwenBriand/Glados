module Lexer (
    Expr(..),
    TokorExpr(..),
    tokWordToExpr
) where

import Tokenizer
import qualified Control.Applicative as ExprDeclareSymbol

data TokorExpr = T TokenInfo
               | E Expr
               deriving (Eq, Show)

data Expr = ExprInteger
          | ExprSymbol
          | ExprDeclareSymbol
          | ExprEmpty
          | ExprSum
          | ExprSub
          | ExprMul
          | ExprDiv
          | ExprMod
          | ExprError
          deriving (Eq, Show)

newtype ASTNode = Node(Expr, [ASTNode]) deriving (Eq, Show)
data NodeorTE = ASTN ASTNode | ASTTmp TokorExpr deriving (Eq, Show)

-- Takes an array of Token and / or expressions and tries to collapse it into
-- a single expression. Returns ExprError upon failure to do so.
tokWordToExpr :: [TokorExpr] -> Expr
tokWordToExpr [] = ExprEmpty
tokWordToExpr [T (TokenInfo TokSymbol _)] = ExprSymbol -- a symbol is a variable name, function name, etc.
tokWordToExpr [T (TokenInfo TokOpenParen _), E e, T (TokenInfo TokCloseParen _)] = e -- an expression between parentheses stays an expression
tokWordToExpr [T (TokenInfo TokInteger _)] = ExprInteger -- an int, has the val of the number iT TokenInfo contains
tokWordToExpr [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeyworddefine _), T (TokenInfo TokSymbol _), E _, T (TokenInfo TokCloseParen _)] = ExprDeclareSymbol -- (define name expr) (define x 2), declares the existence of a symbol and takes the val of thaT TokenInfo symbol.
tokWordToExpr [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorPlus _), E _, E _, T (TokenInfo TokCloseParen _) ] = ExprSum -- (+ 1 2), sums the two expressions. Takes the val of the sum's result.
tokWordToExpr [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMinus _), E _, E _, T (TokenInfo TokCloseParen _) ] = ExprSub -- (- 1 2), substracts the two expressions. Takes the val of the substraction's result.
tokWordToExpr [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMul _), E _, E _, T (TokenInfo TokCloseParen _) ] = ExprMul -- (* 1 2), multiplies the two expressions. Takes the val of the multiplication's result.
tokWordToExpr [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorDiv _), E _, E _, T (TokenInfo TokCloseParen _) ] = ExprDiv -- (/ 1 2), divides the two expressions. Takes the val of the division's result.
tokWordToExpr [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMod _), E _, E _, T (TokenInfo TokCloseParen _) ] = ExprMod -- (% 1 2), takes the modulo of the two expressions. Takes the value of the modulo's result.
tokWordToExpr _ = ExprError

-- nteCollapse :: [NodeorTE] -> NodeorTE

-- | @params:
--     tokArray: the array of tokens to build a tree from
-- @return: a tree of expressions that represent the information contained in the array.
-- buildAST :: [NodeorTE]  -> [ASTNode]
-- buildAST [] = []
-- buildAST (x:xs) =

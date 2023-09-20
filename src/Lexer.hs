module Lexer (
    -- Expr(..),
    -- TokorExpr(..),
    TokorNode(..),
    ASTNode(..),
    tokOrExprToASTNode,
    tryToMatch,
    buildASTIterate,
    buildAST
) where

import Tokenizer

data TokorNode = T TokenInfo
               | A ASTNode
                deriving (Eq, Show)

data ASTNode = ASTNodeError {astnerrToken :: TokenInfo}
             | ASTNodeInteger {astniValue :: Integer}
--  The sum can have an arbitrary number of parameters
             | ASTNodeSum {astnsChildren :: [ASTNode]}
             | ASTNodeSub {astnsChildren :: [ASTNode]}
             | ASTNodeMul {astnsChildren :: [ASTNode]}
             | ASTNodeDiv {astnsChildren :: [ASTNode]}
             | ASTNodeMod {astnsChildren :: [ASTNode]}
             | ASTNodeDebug {astndChildren :: [TokorNode]}
    deriving (Eq, Show)

-- | @params:
--     l: the word which will be compared to the different constructors for nodes.
-- @return: a node corresponding to the pattern given in argument, or an error
-- if the pattern does not match.
tokOrExprToASTNode :: [TokorNode] -> ASTNode
-- error: empty
tokOrExprToASTNode [] = ASTNodeError (TokenInfo TokError "")
-- an integer
tokOrExprToASTNode [T (TokenInfo TokInteger val)] = ASTNodeInteger (read val)
-- a sum of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorPlus _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeSum [n1, n2]
-- a sub of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMinus _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeSub [n1, n2]
-- a mul of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMul _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeMul [n1, n2]
-- a div of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorDiv _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeDiv [n1, n2]
-- a mod of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMod _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeMod [n1, n2]
-- error
tokOrExprToASTNode _ = ASTNodeError (TokenInfo TokError "")



-- | @params:
--     currWord: the current Array which we are trying to reduce to a single node
--     lastmatch: the last valid match. First call the function with ASTNodeError
--     arr: the rest of the array
-- @return: a node, and the rest of the array that has not been consumed
tryToMatch :: [TokorNode] -> TokorNode -> [TokorNode] -> (TokorNode, [TokorNode])
tryToMatch [] _ [] = (A (ASTNodeError (TokenInfo TokError "")), [])
tryToMatch _ lastmatch [] = (lastmatch, [])
tryToMatch currWord lastmatch (x:xs) = case (tokOrExprToASTNode (currWord ++ [x])) of
    ASTNodeError _ -> tryToMatch (currWord ++ [x]) (lastmatch) xs
    anynode -> (A anynode, xs)

-- calls tryToMatch on every index of the array, while updating it
-- with the response to remove the consumed elements
-- | @params:
--     l: the array to reduce
-- @return: an array of tokens and / or nodes
buildASTIterate :: [TokorNode] -> [TokorNode]
buildASTIterate [] = []
buildASTIterate (l:ls) = case tryToMatch [] (T (TokenInfo TokError "")) (l:ls) of
    (A (ASTNodeError _), _) -> l : buildASTIterate ls
    (T (TokenInfo TokError _), []) -> l : buildASTIterate ls
    (something, xs) -> something : buildASTIterate xs


-- calls buildASTIterate in a loop to progressively reduce the array
-- to a single node
-- | @params:
--     l: the array to reduce
-- @return: the root node of the AST
buildAST :: [TokorNode] -> ASTNode
buildAST [] = ASTNodeError (TokenInfo TokError "empty")
buildAST l = case buildASTIterate l of
    [A n] -> n
    [] -> ASTNodeError (TokenInfo TokError "empty")
    (n:ns) -> if l == n:ns
                then ASTNodeError (TokenInfo TokError "cannot resolve input")
                else buildAST (n:ns)

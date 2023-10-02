module Lexer (
    -- Expr(..),
    -- TokorExpr(..),
    TokorNode(..),
    ASTNode(..),
    VarType(..),
    tokOrExprToASTNode,
    tryToMatch,
    buildASTIterate,
    buildAST,
    strToAST
) where

import Tokenizer


data VarType = GUndefinedType
    | GInt -- 64 bit integer
    | GBool -- True or False, #t or #f
    | GVoid -- No return value
    deriving (Show, Eq)

data TokorNode = T TokenInfo
               | A ASTNode
                deriving (Eq, Show)

data ASTNode = ASTNodeError {astnerrToken :: TokenInfo}
             | ASTNodeInteger {astniValue :: Integer}
             | ASTNodeSymbol {astnsName :: String}
             | ASTNodeDefine {astndName :: ASTNode, astndChildren :: ASTNode}
--  The sum can have an arbitrary number of parameters
             | ASTNodeSum {astnsChildren :: [ASTNode]}
             | ASTNodeSub {astnsChildren :: [ASTNode]}
             | ASTNodeMul {astnsChildren :: [ASTNode]}
             | ASTNodeDiv {astnsChildren :: [ASTNode]}
             | ASTNodeMod {astnsChildren :: [ASTNode]}
             | ASTNodeDebug {astndChildrenDebug :: [TokorNode]}
             | ASTNodeParamList {astnplChildren :: [ASTNode]}
             | ASTNodeArray {astnaChildren :: [ASTNode]}
             | ASTNodeInstructionSequence {astnisChildren :: [ASTNode]}
             | ASTNodeBoolean {astnbValue :: Bool}
    deriving (Eq, Show)

-- | @params:
--     l: the word which will be compared to the different constructors for nodes.
-- @return: a node corresponding to the pattern given in argument, or an error
-- if the pattern does not match.
tokOrExprToASTNode :: [TokorNode] -> ASTNode
-- error: empty
tokOrExprToASTNode [] = ASTNodeError (TokenInfo TokError "")
-- param list
tokOrExprToASTNode [A (ASTNodeParamList l), A n] = ASTNodeParamList (l ++ [n])
tokOrExprToASTNode [A n1, A n2] = ASTNodeParamList [n1, n2]
-- array
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), A (ASTNodeParamList l), T (TokenInfo TokCloseParen _)] = ASTNodeArray l
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), A n, T (TokenInfo TokCloseParen _)] = ASTNodeArray [n]
-- an integer
tokOrExprToASTNode [T (TokenInfo TokInteger val)] = ASTNodeInteger (read val)
-- a symbol
tokOrExprToASTNode [T (TokenInfo TokSymbol val)] = ASTNodeSymbol val
-- a sum of expressions
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorPlus _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeSum [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorPlus _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeSum [n1, n2]
-- a sub of expressions
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMinus _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeSub [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMinus _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeSub [n1, n2]
-- a mul of expressionsk
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMul _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeMul [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMul _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeMul [n1, n2]
-- a div of expressions
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorDiv _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeDiv [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorDiv _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeDiv [n1, n2]
-- a mod of expressions
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMod _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeMod [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMod _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeMod [n1, n2]
-- declaration of a variable
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeyworddefine _), A (ASTNodeSymbol sym), A n, T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol sym) n
-- a boolean
tokOrExprToASTNode [T (TokenInfo TokenBool val)] = ASTNodeBoolean (val == "true")
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
tryToMatch currWord lastmatch (x:xs) = case tokOrExprToASTNode (currWord ++ [x]) of
    ASTNodeError _ -> tryToMatch (currWord ++ [x]) lastmatch xs
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

-- Tries to create a list of instructions from a list of nodes. If there are any
-- unresolved tokens, it returns an error.
tryBuildInstructionList :: [TokorNode] -> ASTNode
tryBuildInstructionList [] = ASTNodeError (TokenInfo TokError "empty")
tryBuildInstructionList [A (ASTNodeParamList l)] = ASTNodeInstructionSequence l
tryBuildInstructionList [A (ASTNodeInstructionSequence l)] = ASTNodeInstructionSequence l
tryBuildInstructionList [A (ASTNodeInstructionSequence l), A n] = ASTNodeInstructionSequence (l ++ [n])
tryBuildInstructionList [A n1, A n2] = ASTNodeInstructionSequence [n1, n2]
tryBuildInstructionList _ = ASTNodeError (TokenInfo TokError "cannot resolve input")

-- calls buildASTIterate in a loop to progressively reduce the array
-- to a single node
-- | @params:
--     l: the array to reduce
-- @return: the root node of the AST
buildAST :: [TokorNode] -> ASTNode
buildAST [] = ASTNodeError (TokenInfo TokError "empty")
buildAST l = case buildASTIterate l of
    [A (ASTNodeParamList instr)] -> ASTNodeInstructionSequence instr
    [A n] -> n
    [] -> ASTNodeError (TokenInfo TokError "empty")
    (n:ns) -> if l == n:ns
                -- then ASTNodeError (TokenInfo TokError "cannot resolve input")
                -- then ASTNodeDebug (n:ns)
                then tryBuildInstructionList (n:ns)
                else buildAST (n:ns)

-- | @params:
--     str: the string to convert to an AST
-- @return: the root node of the AST
strToAST :: String -> ASTNode
strToAST str = buildAST (map T (tokenize str))

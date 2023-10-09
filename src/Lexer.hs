{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lexer
  ( -- Expr(..),
    -- TokorExpr(..),
    TokorNode (..),
    ASTNode (..),
    VarType (..),
    tokOrExprToASTNode,
    tryToMatch,
    buildASTIterate,
    buildAST,
    strToAST,
    tryBuildInstructionList,
    typeToInt,
    intToType,
  )
where

import Tokenizer
import ValidState

data VarType
  = GUndefinedType
  | GInt -- 64 bit integer
  | GBool -- True or False, #t or #f
  | GVoid -- No Prelude.return value
  deriving (Show, Eq)

data TokorNode
  = T TokenInfo
  | A ASTNode
  deriving (Eq)

instance Show TokorNode where
  show (T t) = show t
  show (A a) = show a

data ASTNode
  = ASTNodeError {astnerrToken :: TokenInfo}
  | ASTNodeInteger {astniValue :: Integer}
  | ASTNodeSymbol {astnsName :: String}
  | ASTNodeMutable {astndName :: ASTNode, astndChildren :: ASTNode}
  | --  The sum can have an arbitrary number of parameters
    ASTNodeSum {astnsChildren :: [ASTNode]}
  | ASTNodeSub {astnsChildren :: [ASTNode]}
  | ASTNodeMul {astnsChildren :: [ASTNode]}
  | ASTNodeDiv {astnsChildren :: [ASTNode]}
  | ASTNodeMod {astnsChildren :: [ASTNode]}
  | ASTNodeDebug {astndChildrenDebug :: [TokorNode]}
  | ASTNodeParamList {astnplChildren :: [ASTNode]}
  | ASTNodeArray {astnaChildren :: [ASTNode]}
  | ASTNodeInstructionSequence {astnisChildren :: [ASTNode]}
  | ASTNodeBoolean {astnbValue :: Bool}
  | ASTNodeEq {astneChildren :: [ASTNode]}
  | ASTNodeInferior {astniChildren :: [ASTNode]}
  | ASTNodeInferiorEq {astniChildren :: [ASTNode]}
  | ASTNodeSuperior {astniChildren :: [ASTNode]}
  | ASTNodeSuperiorEq {astniChildren :: [ASTNode]}
  | ASTNodeIf {astniCondition :: ASTNode, astniThen :: [ASTNode], astniElse :: ValidState [ASTNode]}
  | ASTNodePrint {astnPrint :: ASTNode}
  | ASTNodeDefine {astndName :: ASTNode, astndParams :: ValidState ASTNode, astndBody :: [ASTNode]}
  | ASTNodeLambda {astndName :: ASTNode, astndParams :: ValidState ASTNode, astndBody :: [ASTNode]}
  | ASTNodeFunctionCall {astnfName :: String, astfnParams :: [ASTNode]}
  | ASTNodeBreak {astneChildren :: [ASTNode]}
  deriving (Eq)

instance Show ASTNode where
  show (ASTNodeError t) = "(Error: " ++ show t ++ ")"
  show (ASTNodeInteger i) = "(int: " ++ show i ++ ")"
  show (ASTNodeSymbol s) = "(sym: " ++ s ++ ")"
  show (ASTNodeMutable n c) = "(define: " ++ show n ++ " \n\t" ++ show c ++ ")"
  show (ASTNodeSum l) = "(sum: " ++ show l ++ ")"
  show (ASTNodeSub l) = "(sub: " ++ show l ++ ")"
  show (ASTNodeMul l) = "(mul: " ++ show l ++ ")"
  show (ASTNodeDiv l) = "(div: " ++ show l ++ ")"
  show (ASTNodeMod l) = "(mod: " ++ show l ++ ")"
  show (ASTNodeDebug l) = "(debug: " ++ show l ++ ")"
  show (ASTNodeParamList l) = "(paramlist: \n\t" ++ show l ++ ")"
  show (ASTNodeArray l) = "(array: {\n\t" ++ show l ++ "\n})"
  show (ASTNodeInstructionSequence l) = "(instructionsequence: \n\t" ++ show l ++ ")"
  show (ASTNodeBoolean b) = "(bool: " ++ show b ++ ")"
  show (ASTNodeIf c t e) = "(if: \n\t(condition) " ++ show c ++ "\n\t(then) " ++ show t ++ "\n\t(else) " ++ show e ++ ")"
  show (ASTNodeDefine n p b) = "(define: \n\t(name) " ++ show n ++ "\n\t(params) " ++ show p ++ "\n\t(body) {" ++ show b ++ "})"
  show (ASTNodePrint p) = "(print " ++ show p ++ ")"
  show (ASTNodeFunctionCall n p) = "(functioncall: \n\t(name) " ++ n ++ "\n\t(params) " ++ show p ++ ")\n"
  show (ASTNodeLambda n p b) = "(lambda: \n\t(name) " ++ show n ++ "\n\t(params) " ++ show p ++ "\n\t(body) {" ++ show b ++ "})\n"
  show (ASTNodeBreak l) = "(break: " ++ show l ++ ")"
  show (ASTNodeEq l) = "(eq: " ++ show l ++ ")"
  show _ = "(unknown node)"

isSymbolAndParamArray :: [ASTNode] -> Bool
isSymbolAndParamArray [ASTNodeSymbol _, _] = True
isSymbolAndParamArray ((ASTNodeSymbol _) : _ : _) = True
isSymbolAndParamArray ((ASTNodeLambda {}) : _) = True
isSymbolAndParamArray _ = False

expendParamList :: [ASTNode] -> [ASTNode]
expendParamList ((ASTNodeArray l) : xs) = expendParamList l ++ expendParamList xs
expendParamList ((ASTNodeParamList l) : xs) = expendParamList l ++ expendParamList xs
expendParamList (x : xs) = x : expendParamList xs
expendParamList [] = []

isThisReallyAnArrayOrIsItATrap :: ASTNode -> ASTNode
isThisReallyAnArrayOrIsItATrap (ASTNodeArray arr) =
  if isSymbolAndParamArray arr
    then case head arr of
      ASTNodeSymbol _ -> ASTNodeFunctionCall (astnsName (head arr)) (tail arr)
      ASTNodeLambda name params body -> ASTNodeBreak [ASTNodeLambda name params body, ASTNodeFunctionCall (astnsName name) (expendParamList (tail arr))]
      _ -> ASTNodeError (TokenInfo TokError "")
    else -- then ASTNodeFunctionCall (astnsName (arr !! 0)) (tail arr)
      ASTNodeArray arr
isThisReallyAnArrayOrIsItATrap a = a

-- | @params:
--     l: the word which will be compared to the different constructors for nodes.
-- @return: a node corresponding to the pattern given in argument, or an error
-- if the pattern does not match.
tokOrExprToASTNode :: [TokorNode] -> ASTNode
-- error: empty
tokOrExprToASTNode [] = ASTNodeError (TokenInfo TokError "")
-- call function with parameters. (no params is just handled by node symbol)
-- tokOrExprToASTNode [A (ASTNodeArray ((ASodeTNodeSymbol sym):params))] = ASTNodeFunctionCall sym params
tokOrExprToASTNode [A (ASTNodeArray [ASTNodeSymbol sym, ASTNodeInteger i])] = ASTNodeFunctionCall sym [ASTNodeInteger i]
-- tokOrExprToASTNode [A (ASTNodeArray [(ASTNodeSymbol s), p])] = ASTNodeFunctionCall s [p]
-- tokOrExprToASTNode [A (ASTNodeArray ((ASTNodeSymbol s):p:ps))] = ASTNodeFunctionCall s (p:ps)

-- declaration of a Lambda
-- lambda with parameters
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokLambda _), A (ASTNodeParamList [ASTNodeFunctionCall param1 params, body]), T (TokenInfo TokCloseParen _)] = ASTNodeLambda (ASTNodeSymbol "") (Valid (ASTNodeParamList (ASTNodeSymbol param1 : params))) [body]
-- lambda with variable
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeSymbol sym), A (ASTNodeLambda _ param body), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol sym) param (expendParamList body)
--end lambda

-- declaration of a Function
-- with parameters
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeSymbol sym), A (ASTNodeArray params), A (ASTNodeArray body), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol sym) (Valid (ASTNodeParamList params)) body
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeParamList [ASTNodeSymbol name, ASTNodeArray params, body]), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol name) (Valid (ASTNodeParamList params)) [body]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeSymbol sym), A (ASTNodeFunctionCall a as), A (ASTNodeArray body), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol sym) (Valid (ASTNodeParamList (ASTNodeSymbol a : as))) body
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeParamList [ASTNodeSymbol name, ASTNodeFunctionCall n ns, body]), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol name) (Valid (ASTNodeParamList (ASTNodeSymbol n : ns))) [body]
-- without parameters
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeSymbol sym), A body, T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol sym) (Invalid (sym ++ " does not take any parameters")) [body]
-- an if statement
-- cond: arr then: arr(1) else: nop
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray [cond]), T (TokenInfo TokenKeywordThen _), A (ASTNodeArray [thenOps]), T (TokenInfo TokCloseParen _)] = ASTNodeIf cond [thenOps] (Invalid "1")
-- cond: arr then: arr else: nop

-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray [cond]), A (ASTNodeArray thenOps), T (TokenInfo TokCloseParen _)] = ASTNodeIf cond thenOps (Invalid "3")
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeParamList [ASTNodeBoolean cond, n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeIf (ASTNodeBoolean cond) [n1] (Valid [n2])
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeParamList [ASTNodeBoolean cond, n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeIf (ASTNodeBoolean cond) [n1] (Valid [n2])
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray [cond]), A (ASTNodeArray thenOps), A (ASTNodeArray elseOps), T (TokenInfo TokCloseParen _)] = ASTNodeIf cond thenOps (Valid elseOps)
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray [cond]), T (TokenInfo TokenKeywordThen _), A (ASTNodeArray thenOps), T (TokenInfo TokCloseParen _)] = ASTNodeIf cond thenOps (Invalid "3")
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray [cond]), T (TokenInfo TokenKeywordThen _), A (ASTNodeArray thenOps), T (TokenInfo TokenKeywordElse _), A (ASTNodeArray elseOps), T (TokenInfo TokCloseParen _)] = ASTNodeIf cond thenOps (Valid elseOps)
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray [cond]), A thenOps, A elseOps, T (TokenInfo TokCloseParen _)] = ASTNodeIf cond [thenOps] (Valid [elseOps])
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray [cond]), T (TokenInfo TokenKeywordThen _), A thenOps, T (TokenInfo TokCloseParen _)] = ASTNodeIf cond [thenOps] (Invalid "3")
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray [cond]), T (TokenInfo TokenKeywordThen _), A thenOps, T (TokenInfo TokenKeywordElse _), A elseOps, T (TokenInfo TokCloseParen _)] = ASTNodeIf cond [thenOps] (Valid [elseOps])
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A cond, T (TokenInfo TokenKeywordThen _), A thenOps, T (TokenInfo TokenKeywordElse _), A elseOps, T (TokenInfo TokCloseParen _)] = ASTNodeIf cond (expendParamList [thenOps]) (Valid (expendParamList [elseOps]))
-- print
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenSymPrint _), A n, T (TokenInfo TokCloseParen _)] = ASTNodePrint n
-- array
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), A (ASTNodeParamList l), T (TokenInfo TokCloseParen _)] = isThisReallyAnArrayOrIsItATrap (ASTNodeArray l)
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), A n, T (TokenInfo TokCloseParen _)] = isThisReallyAnArrayOrIsItATrap (ASTNodeArray [n])
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
-- a mul of expressions
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMul _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeMul [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMul _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeMul [n1, n2]
-- a div of expressions
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorDiv _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeDiv [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorDiv _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeDiv [n1, n2]
-- a eq? of expressions
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenEqual _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeEq [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenEqual _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeEq [n1, n2]
-- a < of expressions
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenInferior _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeInferior [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenInferior _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeInferior [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenInferiorEq _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeInferiorEq [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenSuperior _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeSuperior [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenSuperiorEq _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeSuperiorEq [n1, n2]
-- a mod of expressions
-- tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMod _), A n1, A n2, T (TokenInfo TokCloseParen _)] = ASTNodeMod [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMod _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeMod [n1, n2]
-- declaration of a variable
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordMutable _), A (ASTNodeSymbol sym), A n, T (TokenInfo TokCloseParen _)] = ASTNodeMutable (ASTNodeSymbol sym) (ASTNodeArray (expendParamList [n]))
-- a boolean
tokOrExprToASTNode [T (TokenInfo TokenBool val)] = ASTNodeBoolean (val == "true")
-- param list
tokOrExprToASTNode [A (ASTNodeParamList l), A n] = ASTNodeParamList (l ++ [n])
tokOrExprToASTNode [A n1, A n2] = ASTNodeParamList [n1, n2]
-- error
tokOrExprToASTNode unresolved = ASTNodeError (TokenInfo TokError (show unresolved))

typeToInt :: VarType -> Int
typeToInt GUndefinedType = 1
typeToInt GInt = 2
typeToInt GBool = 3
typeToInt GVoid = 4
typeToInt _ = 0

intToType :: ValidState Int -> ValidState VarType
intToType (Invalid s) = Invalid s
intToType (Valid i) = case i of
  0 -> Valid GUndefinedType
  1 -> Valid GUndefinedType
  2 -> Valid GInt
  3 -> Valid GBool
  4 -> Valid GVoid
  _ -> Invalid "invalid type"

-- | @params:
--     currWord: the current Array which we are trying to reduce to a single node
--     lastmatch: the last valid match. First call the function with ASTNodeError
--     arr: the rest of the array
-- @return: a node, and the rest of the array that has not been consumed
tryToMatch :: [TokorNode] -> TokorNode -> [TokorNode] -> (TokorNode, [TokorNode])
tryToMatch [] _ [] = (A (ASTNodeError (TokenInfo TokError "")), [])
tryToMatch _ lastmatch [] = (lastmatch, [])
tryToMatch currWord lastmatch (x : xs) = case tokOrExprToASTNode (currWord ++ [x]) of
  ASTNodeError _ -> tryToMatch (currWord ++ [x]) lastmatch xs
  anynode -> (A anynode, xs)

-- calls tryToMatch on every index of the array, while updating it
-- with the response to remove the consumed elements

-- | @params:
--     l: the array to reduce
-- @return: an array of tokens and / or nodes
buildASTIterate :: [TokorNode] -> [TokorNode]
buildASTIterate [] = []
buildASTIterate (l : ls) = case tryToMatch [] (T (TokenInfo TokError "")) (l : ls) of
  (A (ASTNodeError _), _) -> l : buildASTIterate ls
  (T (TokenInfo TokError _), []) -> l : buildASTIterate ls
  (something, xs) -> something : buildASTIterate xs

-- Tries to create a list of instructions from a list of nodes. If there are any
-- unresolved tokens, it Prelude.returns an error.
tryBuildInstructionList :: [TokorNode] -> ASTNode
tryBuildInstructionList [] = ASTNodeError (TokenInfo TokError "empty")
tryBuildInstructionList [A (ASTNodeParamList l)] = ASTNodeInstructionSequence l
tryBuildInstructionList [A (ASTNodeInstructionSequence l)] = ASTNodeInstructionSequence l
tryBuildInstructionList [A (ASTNodeInstructionSequence l), A n] = ASTNodeInstructionSequence (l ++ [n])
tryBuildInstructionList [A n1, A n2] = ASTNodeInstructionSequence [n1, n2]
tryBuildInstructionList l = ASTNodeError (TokenInfo TokError (show l))

-- tryBuildInstructionList _ = ASTNodeError (TokenInfo TokError "cannot resolve input")

-- calls buildASTIterate in a loop to progressively reduce the array
-- to a single node

expandParamLists :: [ASTNode] -> [ASTNode]
expandParamLists ((ASTNodeParamList l) : xs) = l ++ expandParamLists xs
expandParamLists (x : xs) = x : expandParamLists xs
expandParamLists [] = []

instructionSequenceExpandParamList :: ASTNode -> ASTNode
instructionSequenceExpandParamList (ASTNodeInstructionSequence l) = ASTNodeInstructionSequence (expandParamLists l)
instructionSequenceExpandParamList n = n

-- | @params:
--     l: the array to reduce
-- @return: the root node of the AST
buildAST :: [TokorNode] -> ASTNode
buildAST [] = ASTNodeError (TokenInfo TokError "empty")
buildAST l = case buildASTIterate l of
  [A (ASTNodeParamList instr)] -> ASTNodeInstructionSequence instr
  [A n] -> n
  ns ->
    if l == ns
      then -- then ASTNodeError (TokenInfo TokError "cannot resolve input")
      -- then ASTNodeDebug (n:ns)
        tryBuildInstructionList ns
      else buildAST ns

-- | @params:
--     str: the string to convert to an AST
-- @return: the root node of the AST
strToAST :: String -> ASTNode
strToAST str = instructionSequenceExpandParamList (buildAST (map T (tokenize str)))

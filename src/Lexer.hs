{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE DeriveGeneric #-}


module Lexer
  (
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
    expendParamList,
    getTypeFromNodeValue,
    getTypeFromToken,
    expandParamLists,
    mergeBinOps,
    isSymbolAndParamArray,
    expendParamList,
    isThisReallyAnArrayOrIsItATrap,
    getTypeFromToken,
    getTypeFromNodeValue,
    typeToInt,
    intToType,
    instructionSequenceExpandParamList,
    containsParamList
  )
where

import Tokenizer
import ValidState

import Data.Binary
import GHC.Generics (Generic)

data VarType
  = GUndefinedType
  | GInt 
  | GBool 
  | GVoid 
  | GPtr 
  | GStruct String 
  deriving (Show, Eq, Generic)

instance Binary VarType

data TokorNode
  = T TokenInfo
  | A ASTNode
  deriving (Eq, Generic)

instance Binary TokorNode

instance Show TokorNode where
  show (T t) = show t
  show (A a) = show a

data ASTNode
  = ASTNodeError {astnerrToken :: TokenInfo}
  | ASTNodeInteger {astniValue :: Integer}
  | ASTNodeSymbol {astnsName :: String}
  | ASTNodeMutable {astnmSymType :: ASTNode, astnmName :: ASTNode, astnmValueType :: ASTNode, astnmChildren :: ASTNode}
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
  | ASTNodeEq {astneChildren :: [ASTNode]}
  | ASTNodeInferior {astniChildren :: [ASTNode]}
  | ASTNodeInferiorEq {astniChildren :: [ASTNode]}
  | ASTNodeSuperior {astniChildren :: [ASTNode]}
  | ASTNodeSuperiorEq {astniChildren :: [ASTNode]}
  | ASTNodeNotEqual {astneChildren :: [ASTNode]}
  | ASTNodeIf {astniCondition :: ASTNode, astniThen :: [ASTNode], astniElse :: ValidState [ASTNode]}
  | ASTNodeElif {astniCondition :: ASTNode, astniThen :: [ASTNode], astniElse :: ValidState [ASTNode]}
  | ASTNodeElse {astniThen :: [ASTNode]}
  | ASTNodePrint {astnPrint :: ASTNode}
  | ASTNodeDefine {astndName :: ASTNode, astndParams :: ValidState ASTNode, astndBody :: [ASTNode]}
  | ASTNodeLambda {astndName :: ASTNode, astndParams :: ValidState ASTNode, astndBody :: [ASTNode]}
  | ASTNodeCapture {astncChildren :: [ASTNode]}
  | ASTNodeFunctionCall {astnfName :: String, astfnParams :: [ASTNode]}
  | ASTNodeBreak {astneChildren :: [ASTNode]}
  | ASTNodeWhile {astniCondition :: ASTNode, astniThen :: [ASTNode]}
  | ASTNodeSet {astndName :: ASTNode, astndChildren :: ASTNode}
  | ASTNodeType {astntName :: VarType}
  | ASTNodeVariable {astnvName :: ASTNode, astnvType :: VarType}
  | ASTNodeReturn {astnrValue :: ASTNode}
  | ASTNodeDeref {astndChildren :: ASTNode, astndindex :: ASTNode}
  | ASTNodeCast {astncastee :: ASTNode, astncasttype :: VarType}
  | ASTNodeBinOps {astnboChildren :: [TokorNode]}
  | ASTNodeShow {astnsChildren :: [ASTNode], astnsType :: VarType}
  | ASTNodeStruct {astnsName :: String, astnsChildren :: [ASTNode]}
  | ASTNodeStructVariable {astnName :: ASTNode, astnVar :: ASTNode}
  deriving (Eq, Generic)

instance Binary ASTNode

instance Show ASTNode where
  show (ASTNodeError t) = "(Error: " ++ show t ++ ")"
  show (ASTNodeInteger i) = "(int: " ++ show i ++ ")"
  show (ASTNodeSymbol s) = "(sym: " ++ s ++ ")"
  show (ASTNodeMutable tn n tv v) = "(mutable: \n\t(symbol type) " ++ show tn ++ "\n\t(symbol name)" ++ show n ++ "\n\t(value type)" ++ show tv ++ " \n\t(value)" ++ show v ++ ")\n"
  show (ASTNodeSum l) = "(add: " ++ show l ++ ")"
  show (ASTNodeSub l) = "(sub: " ++ show l ++ ")"
  show (ASTNodeMul l) = "(mul: " ++ show l ++ ")"
  show (ASTNodeDiv l) = "(div: " ++ show l ++ ")"
  show (ASTNodeMod l) = "(mod: " ++ show l ++ ")"
  show (ASTNodeDebug l) = "(debug: " ++ show l ++ ")"
  show (ASTNodeParamList l) = "(paramlist: \n\t" ++ show l ++ ")\n"
  show (ASTNodeArray l) = "(array: {\n\t" ++ show l ++ "\n})"
  show (ASTNodeInstructionSequence l) = "(instructionsequence: \n\t" ++ show l ++ ")"
  show (ASTNodeBoolean b) = "(bool: " ++ show b ++ ")"
  show (ASTNodeIf c t e) = "(if: \n\t(condition) " ++ show c ++ "\n\t(then) " ++ show t ++ "\n\t(else) " ++ show e ++ ")"
  show (ASTNodeElif c t e) = "(elsif: \n\t(condition) " ++ show c ++ "\n\t(then) " ++ show t ++ "\n\t(else) " ++ show e ++ ")"
  show (ASTNodeElse t) = "(else: \n\t(then) " ++ show t ++ ")"
  show (ASTNodeDefine n p b) = "(define: \n\t(name) " ++ show n ++ "\n\t(params) " ++ show p ++ "\n\t(body) {" ++ show b ++ "})\n"
  show (ASTNodePrint p) = "(print " ++ show p ++ ")"
  show (ASTNodeFunctionCall n p) = "(functioncall: \n\t(name) " ++ n ++ "\n\t(params) " ++ show p ++ ")\n"
  show (ASTNodeLambda n p b) = "(lambda: \n\t(name) " ++ show n ++ "\n\t(params) " ++ show p ++ "\n\t(body) {" ++ show b ++ "})\n"
  show (ASTNodeCapture c) = "(capture: " ++ show c ++ ")"
  show (ASTNodeBreak l) = "(break: " ++ show l ++ ")"
  show (ASTNodeEq l) = "(eq: " ++ show l ++ ")"
  show (ASTNodeInferior l) = "(inferior: " ++ show l ++ ")"
  show (ASTNodeWhile c t) = "(while: \n\t(condition) " ++ show c ++ "\n\t(then) " ++ show t ++ ")"
  show (ASTNodeSet n c) = "(set: \n\t(name) " ++ show n ++ "\n\t(children) " ++ show c ++ ")"
  show (ASTNodeType n) = "(type: " ++ show n ++ ")"
  show (ASTNodeVariable n t) = "(variable: \n\t(name) " ++ show n ++ "\n\t(type) " ++ show t ++ ")\n"
  show (ASTNodeReturn v) = "(return: " ++ show v ++ ")"
  show (ASTNodeCast n t) = "(cast: \n\t(castee) " ++ show n ++ "\n\t(type) " ++ show t ++ ")"
  show (ASTNodeDeref n i) = "(deref: \n\t(name) " ++ show n ++ "\n\t(index) " ++ show i ++ ")"
  show (ASTNodeBinOps l) = "(binops: " ++ show l ++ ")"
  show (ASTNodeShow l t) = "(show: \n\t(type) " ++ show t ++ "\n\t(children) " ++ show l ++ ")"
  show (ASTNodeStruct n l) = "(struct: \n\t(name) " ++ n ++ "\n\t(children) " ++ show l ++ ")"
  show (ASTNodeStructVariable n l) = "(structvariable: \n\t(name) " ++ show n ++ "\n\t(children) " ++ show l ++ ")"
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
    else
      ASTNodeArray arr
isThisReallyAnArrayOrIsItATrap a = a

getTypeFromToken :: TokenInfo -> VarType
getTypeFromToken (TokenInfo TokenType "int") = GInt
getTypeFromToken (TokenInfo TokenType "bool") = GBool
getTypeFromToken (TokenInfo TokenType "void") = GVoid
getTypeFromToken (TokenInfo TokenType "undefined") = GUndefinedType
getTypeFromToken (TokenInfo TokenType "@") = GPtr
getTypeFromToken (TokenInfo TokenKeywordStruct s) = GStruct s
getTypeFromToken _ = GUndefinedType

getTypeFromNodeValue :: ASTNode -> VarType
getTypeFromNodeValue (ASTNodeInteger _) = GInt
getTypeFromNodeValue (ASTNodeBoolean _) = GBool
getTypeFromNodeValue (ASTNodeArray _) = GPtr
getTypeFromNodeValue (ASTNodeStruct s _) = GStruct s
getTypeFromNodeValue (ASTNodeCast _ t) = t
getTypeFromNodeValue _ = GUndefinedType

mergeBinOps :: [TokorNode] -> ASTNode
mergeBinOps [A (ASTNodeBinOps l)] = ASTNodeBinOps l
mergeBinOps [A (ASTNodeBinOps l1), operator, A (ASTNodeBinOps l2)] = ASTNodeBinOps (l1 ++ [operator] ++ l2)
mergeBinOps [A (ASTNodeBinOps l), operator, A n2] = ASTNodeBinOps (l ++ [operator, A n2])
mergeBinOps [A n1, operator, A (ASTNodeBinOps l)] = ASTNodeBinOps ([A n1, operator] ++ l)
mergeBinOps [A n1, operator, A n2] = ASTNodeBinOps [A n1, operator, A n2]
mergeBinOps other = ASTNodeError (TokenInfo TokError ("Invalid binary operation: " ++ show other))

tokOrExprToASTNode :: [TokorNode] -> ASTNode
tokOrExprToASTNode [] = ASTNodeError (TokenInfo TokError "")

tokOrExprToASTNode [T (TokenInfo TokenKeywordStruct _), A (ASTNodeSymbol name), T (TokenInfo TokOpenCurrBrac _), A (ASTNodeParamList params), T (TokenInfo TokCloseCurrBrac _)] = ASTNodeStruct name params
tokOrExprToASTNode [T (TokenInfo TokenKeywordStruct _), A (ASTNodeSymbol name), T (TokenInfo TokOpenCurrBrac _), A (ASTNodeVariable nameV typV), T (TokenInfo TokCloseCurrBrac _)] = ASTNodeStruct name [(ASTNodeVariable nameV typV)]
tokOrExprToASTNode [A (ASTNodeSymbol name), T (TokenInfo TokenPoint _), A (ASTNodeSymbol variable)] = ASTNodeStructVariable (ASTNodeSymbol name) (ASTNodeSymbol variable)

tokOrExprToASTNode [T (TokenInfo TokenKeywordElse _), T (TokenInfo TokOpenCurrBrac _), A body, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeElse [body]
tokOrExprToASTNode [T (TokenInfo TokenElif _), A (ASTNodeArray cond) , T (TokenInfo TokOpenCurrBrac _), A body, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeElif (head cond) [body] (Invalid "2")
tokOrExprToASTNode [T (TokenInfo TokenElif _), T (TokenInfo TokOpenParen _), A cond, T (TokenInfo TokCloseParen _), T (TokenInfo TokOpenCurrBrac _), A body, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeElif cond [body] (Invalid "1")
tokOrExprToASTNode [T (TokenInfo TokenKeywordIf _), A (ASTNodeArray cond), T (TokenInfo TokOpenCurrBrac _), A thenOps, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeIf (head cond) [thenOps] (Invalid "1")
tokOrExprToASTNode [T (TokenInfo TokenKeywordIf _), A cond, T (TokenInfo TokOpenCurrBrac _), A thenOps, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeIf cond [thenOps] (Invalid "2")

tokOrExprToASTNode [A (ASTNodeElif cond thenOps elseOps), A (ASTNodeElse elseOps2)] = ASTNodeElif cond thenOps (Valid elseOps2)
tokOrExprToASTNode [A (ASTNodeElif cond thenOps elseOps), A (ASTNodeElif cond2 thenOps2 (Valid elseOps2))] = ASTNodeElif cond thenOps (Valid [ASTNodeElif cond2 thenOps2 (Valid elseOps2)])
tokOrExprToASTNode [A (ASTNodeIf cond thenOps elseOps), A (ASTNodeElif cond2 thenOps2 (Valid elseOps2))] = ASTNodeIf cond thenOps (Valid [ASTNodeElif cond2 thenOps2 (Valid elseOps2)])
tokOrExprToASTNode [A (ASTNodeIf cond thenOps elseOps), A (ASTNodeElse elseOps2)] = ASTNodeIf cond thenOps (Valid elseOps2)

tokOrExprToASTNode [T (TokenInfo TokenKeywordWhile _), A cond, T (TokenInfo TokOpenCurrBrac _), A thenOps, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeWhile cond [thenOps]
tokOrExprToASTNode [T (TokenInfo TokenKeywordWhile _), A (ASTNodeArray cond), T (TokenInfo TokOpenCurrBrac _), A thenOps, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeWhile (head cond) [thenOps]

tokOrExprToASTNode [T (TokenInfo TokenKeywordFor _), T (TokenInfo TokOpenParen _), A (ASTNodeMutable symtyp name valtyp value), A cond, T (TokenInfo TokenPointComma _), A (ASTNodeSet name2 value2), T (TokenInfo TokCloseParen _), T (TokenInfo TokOpenCurrBrac _), A thenOps, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeInstructionSequence [ASTNodeMutable symtyp name valtyp value, ASTNodeWhile cond [ASTNodeInstructionSequence [thenOps, ASTNodeSet name2 value2]]]
tokOrExprToASTNode [T (TokenInfo TokenKeywordFor _), T (TokenInfo TokOpenParen _), A (ASTNodeParamList [ASTNodeMutable symtyp name valtyp value, cond]), T (TokenInfo TokenPointComma _), A (ASTNodeSet name2 value2), T (TokenInfo TokCloseParen _), T (TokenInfo TokOpenCurrBrac _), A thenOps, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeInstructionSequence [ASTNodeMutable symtyp name valtyp value, ASTNodeWhile cond [ASTNodeInstructionSequence [thenOps, ASTNodeSet name2 value2]]]


tokOrExprToASTNode [A (ASTNodeSymbol name), T (TokenInfo TokenEq _), A n, T (TokenInfo TokenPointComma _)] = ASTNodeSet (ASTNodeSymbol name) n
tokOrExprToASTNode [A (ASTNodeStructVariable name variable), T (TokenInfo TokenEq _), A n, T (TokenInfo TokenPointComma _)] = ASTNodeSet (ASTNodeStructVariable name variable) n

tokOrExprToASTNode [A (ASTNodeSymbol name), T (TokenInfo TokOperatorPlus _), T (TokenInfo TokOperatorPlus _), T (TokenInfo TokenPointComma _)] = ASTNodeSet (ASTNodeSymbol name) (ASTNodeSum [ASTNodeSymbol name, ASTNodeInteger 1])
tokOrExprToASTNode [A (ASTNodeSymbol name), T (TokenInfo TokOperatorMinus _), T (TokenInfo TokOperatorMinus _), T (TokenInfo TokenPointComma _)] = ASTNodeSet (ASTNodeSymbol name) (ASTNodeSub [ASTNodeSymbol name, ASTNodeInteger 1])

tokOrExprToASTNode [A n1, T (TokenInfo TokOperatorMul _), A n2] =  mergeBinOps [A n1, T (TokenInfo TokOperatorMul ""), A n2]
tokOrExprToASTNode [A n1, T (TokenInfo TokOperatorDiv _), A n2] =  mergeBinOps [A n1, T (TokenInfo TokOperatorDiv ""), A n2]
tokOrExprToASTNode [A n1, T (TokenInfo TokOperatorMod _), A n2] =  mergeBinOps [A n1, T (TokenInfo TokOperatorMod ""), A n2]
tokOrExprToASTNode [A n1, T (TokenInfo TokOperatorPlus _), A n2] = mergeBinOps [A n1, T (TokenInfo TokOperatorPlus ""), A n2]
tokOrExprToASTNode [A n1, T (TokenInfo TokOperatorMinus _), A n2] = mergeBinOps [A n1, T (TokenInfo TokOperatorMinus ""), A n2]

tokOrExprToASTNode [T (TokenInfo TokenSymPrint _), T (TokenInfo TokOpenParen _), A n, T (TokenInfo TokCloseParen _), T (TokenInfo TokenPointComma _)] = ASTNodePrint n
tokOrExprToASTNode [T (TokenInfo TokenSymPrint _), T (TokenInfo TokOpenParen _), A n, T (TokenInfo TokCloseParen _)] = ASTNodePrint n
tokOrExprToASTNode [A (ASTNodePrint n), T (TokenInfo TokenPointComma _)] = ASTNodePrint n


tokOrExprToASTNode [T (TokenInfo TokOpenParen _), A n1, T (TokenInfo TokenInferior _), A n2, T (TokenInfo TokCloseParen _)] = ASTNodeInferior [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), A n1, T (TokenInfo TokenInferiorEq _), A n2, T (TokenInfo TokCloseParen _)] = ASTNodeInferiorEq [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), A n1, T (TokenInfo TokenSuperior _), A n2, T (TokenInfo TokCloseParen _)] = ASTNodeSuperior [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), A n1, T (TokenInfo TokenSuperiorEq _), A n2, T (TokenInfo TokCloseParen _)] = ASTNodeSuperiorEq [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), A n1, T (TokenInfo TokenEqual _), A n2, T (TokenInfo TokCloseParen _)] = ASTNodeEq [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), A n1, T (TokenInfo TokenNotEqual _), A n2, T (TokenInfo TokCloseParen _)] = ASTNodeNotEqual [n1, n2]

tokOrExprToASTNode [A (ASTNodeVariable sym typ), T (TokenInfo TokenEq _), A n, T (TokenInfo TokenPointComma _)] = ASTNodeMutable (ASTNodeType typ) sym (ASTNodeType (getTypeFromNodeValue n)) n
tokOrExprToASTNode [A (ASTNodeMutable typ sym valtyp val), T (TokenInfo TokenPointComma _)] = ASTNodeMutable typ sym valtyp val
tokOrExprToASTNode [A (ASTNodeVariable sym typ), T (TokenInfo TokenEq _), A n] = ASTNodeMutable (ASTNodeType typ) sym (ASTNodeType (getTypeFromNodeValue n)) n

tokOrExprToASTNode [A (ASTNodeSymbol sym), T (TokenInfo TokOpenParen _), A (ASTNodeParamList l), T (TokenInfo TokCloseParen _), T (TokenInfo TokenPointComma _)] = ASTNodeFunctionCall sym l
tokOrExprToASTNode [A (ASTNodeSymbol sym), T (TokenInfo TokOpenParen _), A n, T (TokenInfo TokCloseParen _), T (TokenInfo TokenPointComma _)] = ASTNodeFunctionCall sym [n]

-- functions
tokOrExprToASTNode [A (ASTNodeVariable sym typ), T (TokenInfo TokOpenParen _), A n, T (TokenInfo TokCloseParen _), T (TokenInfo TokOpenCurrBrac _), A body, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeDefine sym (Valid (ASTNodeParamList [n])) [body]
tokOrExprToASTNode [A (ASTNodeParamList [ASTNodeVariable sym typ, ASTNodeArray params]), T (TokenInfo TokOpenCurrBrac _), A body, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeDefine sym (Valid (ASTNodeParamList params)) [body]
tokOrExprToASTNode [A (ASTNodeVariable sym typ), A (ASTNodeArray params), T (TokenInfo TokOpenCurrBrac _), A body, T (TokenInfo TokCloseCurrBrac _)] = ASTNodeDefine sym (Valid (ASTNodeParamList params)) [body]

-- lambda with parameters
tokOrExprToASTNode [T (TokenInfo TokOpenBrac _), T (TokenInfo TokCloseBrac _)] = ASTNodeCapture []
tokOrExprToASTNode [A (ASTNodeVariable sym typ), T (TokenInfo TokenEq _), T (TokenInfo TokLambda _), T (TokenInfo TokOpenParen _), A n, T (TokenInfo TokCloseParen _), T (TokenInfo TokOpenCurrBrac _), A body, T (TokenInfo TokCloseCurrBrac _), T (TokenInfo TokenPointComma _)] = ASTNodeDefine sym (Valid (ASTNodeParamList [n])) [body]
tokOrExprToASTNode [A (ASTNodeVariable sym typ), T (TokenInfo TokenEq _), T (TokenInfo TokLambda _), A (ASTNodeArray params), T (TokenInfo TokOpenCurrBrac _), A body, T (TokenInfo TokCloseCurrBrac _), T (TokenInfo TokenPointComma _)] = ASTNodeDefine sym (Valid (ASTNodeParamList params)) [body]

tokOrExprToASTNode [T (TokenInfo TokenReturn _), A n, T (TokenInfo TokenPointComma _)] = ASTNodeReturn n

tokOrExprToASTNode [T (TokenInfo TokenType typ), A (ASTNodeSymbol sym)] = ASTNodeVariable (ASTNodeSymbol sym) (getTypeFromToken (TokenInfo TokenType typ))

-- structures
tokOrExprToASTNode [T (TokenInfo TokenKeywordStruct _), T (TokenInfo TokSymbol sym), T (TokenInfo TokOpenCurrBrac _), A (ASTNodeParamList n), T (TokenInfo TokCloseCurrBrac _), T (TokenInfo TokenPointComma _)] = ASTNodeStruct sym n

tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMinus _), A (ASTNodeInteger i), T (TokenInfo TokCloseParen _)] = ASTNodeInteger (-i)
tokOrExprToASTNode [A (ASTNodeArray [ASTNodeSymbol sym, ASTNodeInteger i])] = ASTNodeFunctionCall sym [ASTNodeInteger i]

-- declaration of a Lambda
-- lambda with parameters
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokLambda _), A (ASTNodeParamList [ASTNodeFunctionCall param1 params, body]), T (TokenInfo TokCloseParen _)] = ASTNodeLambda (ASTNodeSymbol "") (Valid (ASTNodeParamList (ASTNodeSymbol param1 : params))) [body]
-- lambda with variable
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeSymbol sym), A (ASTNodeLambda _ param body), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol sym) param (expendParamList body)
--end lambda

-- declaration of a Function
-- with parameters
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeSymbol sym), A (ASTNodeArray params), A (ASTNodeArray body), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol sym) (Valid (ASTNodeParamList params)) body
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeParamList [ASTNodeSymbol name, ASTNodeArray params, body]), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol name) (Valid (ASTNodeParamList params)) [body]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeSymbol sym), A (ASTNodeFunctionCall a as), A (ASTNodeArray body), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol sym) (Valid (ASTNodeParamList (ASTNodeSymbol a : as))) body
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeParamList [ASTNodeSymbol name, ASTNodeFunctionCall n ns, body]), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol name) (Valid (ASTNodeParamList (ASTNodeSymbol n : ns))) [body]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeParamList [ASTNodeFunctionCall name param, body]), T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol name) (Valid (ASTNodeParamList param)) [body]
-- without parameters
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordDefine _), A (ASTNodeSymbol sym), A body, T (TokenInfo TokCloseParen _)] = ASTNodeDefine (ASTNodeSymbol sym) (Invalid (sym ++ " does not take any parameters")) [body]
-- an if statement
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeParamList [ASTNodeBoolean cond, n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeIf (ASTNodeBoolean cond) [n1] (Valid [n2])
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeParamList [ASTNodeBoolean cond, n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeIf (ASTNodeBoolean cond) [n1] (Valid [n2])
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeParamList [cond, ASTNodeParamList [n1, n2]]), T (TokenInfo TokCloseParen _)] = ASTNodeIf cond [n1] (Valid [n2])
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeParamList [cond, n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeIf cond [n1] (Valid [n2])
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray cond), A (ASTNodeArray thenOps), A (ASTNodeArray elseOps), T (TokenInfo TokCloseParen _)] = ASTNodeIf (head cond) thenOps (Valid elseOps)
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray cond), T (TokenInfo TokenKeywordThen _), A (ASTNodeArray thenOps), T (TokenInfo TokCloseParen _)] = ASTNodeIf (head cond) thenOps (Invalid "3")
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray cond), T (TokenInfo TokenKeywordThen _), A (ASTNodeArray thenOps), T (TokenInfo TokenKeywordElse _), A (ASTNodeArray elseOps), T (TokenInfo TokCloseParen _)] = ASTNodeIf (head cond) thenOps (Valid elseOps)
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray cond), A thenOps, A elseOps, T (TokenInfo TokCloseParen _)] = ASTNodeIf (head cond) [thenOps] (Valid [elseOps])
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray cond), T (TokenInfo TokenKeywordThen _), A thenOps, T (TokenInfo TokCloseParen _)] = ASTNodeIf (head cond) [thenOps] (Invalid "3")
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenKeywordIf _), A (ASTNodeArray cond), T (TokenInfo TokenKeywordThen _), A thenOps, T (TokenInfo TokenKeywordElse _), A elseOps, T (TokenInfo TokCloseParen _)] = ASTNodeIf (head cond) [thenOps] (Valid [elseOps])
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
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorPlus _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeSum [n1, n2]
-- a sub of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMinus _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeSub [n1, n2]
-- a mul of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMul _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeMul [n1, n2]
-- a div of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorDiv _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeDiv [n1, n2]
-- a eq? of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenEqual _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeEq [n1, n2]
-- a < of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenInferior _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeInferior [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenInferiorEq _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeInferiorEq [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenSuperior _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeSuperior [n1, n2]
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokenSuperiorEq _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeSuperiorEq [n1, n2]
-- a mod of expressions
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokOperatorMod _), A (ASTNodeParamList [n1, n2]), T (TokenInfo TokCloseParen _)] = ASTNodeMod [n1, n2]
-- declaration of a variable
tokOrExprToASTNode [T (TokenInfo TokOpenParen _), T (TokenInfo TokKeywordMutable _), A (ASTNodeSymbol sym), A n, T (TokenInfo TokCloseParen _)] = ASTNodeMutable (ASTNodeType (getTypeFromNodeValue n)) (ASTNodeSymbol sym) (ASTNodeType (getTypeFromNodeValue n)) n
-- a boolean
tokOrExprToASTNode [T (TokenInfo TokenBool val)] = ASTNodeBoolean (val == "true")
-- param list
tokOrExprToASTNode [A (ASTNodeIf _ _ _), A (ASTNodeElif _ _ _)] = ASTNodeError (TokenInfo TokError "cannot resolve input")
tokOrExprToASTNode [A (ASTNodeElif _ _ _), A (ASTNodeElif _ _ _)] = ASTNodeError (TokenInfo TokError "cannot resolve input")
tokOrExprToASTNode [A (ASTNodeSet _ _), A (ASTNodeSymbol _)] = ASTNodeError (TokenInfo TokError "cannot resolve input")
tokOrExprToASTNode [A (ASTNodeVariable _ _), A (ASTNodeArray _)] = ASTNodeError (TokenInfo TokError "cannot resolve input")
tokOrExprToASTNode [A (ASTNodeReturn _), A n] = ASTNodeError (TokenInfo TokError "cannot resolve input")
tokOrExprToASTNode [A n1, T (TokenInfo TokenComma _), A n2] = ASTNodeParamList [n1, n2]
tokOrExprToASTNode [A (ASTNodeParamList l), T (TokenInfo TokenComma _), A n] = ASTNodeParamList (l ++ [n])
tokOrExprToASTNode [A (ASTNodeParamList l), A n] = ASTNodeParamList (l ++ [n])
tokOrExprToASTNode [A n1, A n2] = ASTNodeParamList [n1, n2]
-- show with cast
tokOrExprToASTNode [T (TokenInfo TokenShowKeyword _), A n, T (TokenInfo TokenAsKeyword _), T (TokenInfo TokenType typ), T (TokenInfo TokenPointComma _)] = case A n of
        (A (ASTNodeParamList l)) -> ASTNodeShow l (getTypeFromToken (TokenInfo TokenType typ))
        (A (ASTNodeArray l)) -> ASTNodeShow l (getTypeFromToken (TokenInfo TokenType typ))
        (A (ASTNodeInteger _)) -> ASTNodeShow [n] (getTypeFromToken (TokenInfo TokenType typ))
        (A (ASTNodeBoolean _)) -> ASTNodeShow [n] (getTypeFromToken (TokenInfo TokenType typ))
        A n' -> ASTNodeShow [n'] (getTypeFromToken (TokenInfo TokenType typ))

tokOrExprToASTNode [A n, T (TokenInfo TokenCast _), T (TokenInfo TokenType typ)] = ASTNodeCast n (getTypeFromToken (TokenInfo TokenType typ))

tokOrExprToASTNode [A n, T (TokenInfo TokenDeref _), A i] = ASTNodeDeref n i
-- error

tokOrExprToASTNode [T (TokenInfo TokenShowKeyword _), A (ASTNodeCast i t) , T (TokenInfo TokenPointComma _)] = ASTNodeShow [i] t

tokOrExprToASTNode unresolved = ASTNodeError (TokenInfo TokError (show unresolved))

typeToInt :: VarType -> Int
typeToInt GUndefinedType = 1
typeToInt GInt = 2
typeToInt GBool = 3
typeToInt GVoid = 4
typeToInt GPtr = 5
typeToInt (GStruct _) = 6
typeToInt _ = 0

intToType :: ValidState Int -> ValidState VarType
intToType (Invalid s) = Invalid s
intToType (Valid i) = case i of
  0 -> Valid GUndefinedType
  1 -> Valid GUndefinedType
  2 -> Valid GInt
  3 -> Valid GBool
  4 -> Valid GVoid
  5 -> Valid GPtr
  6 -> Valid (GStruct "")
  _ -> Invalid "invalid type"

tryToMatch :: [TokorNode] -> TokorNode -> [TokorNode] -> (TokorNode, [TokorNode])
tryToMatch [] _ [] = (A (ASTNodeError (TokenInfo TokError "")), [])
tryToMatch _ lastmatch [] = (lastmatch, [])
tryToMatch currWord lastmatch (x : xs) = case tokOrExprToASTNode (currWord ++ [x]) of
  ASTNodeError _ -> tryToMatch (currWord ++ [x]) lastmatch xs
  anynode -> (A anynode, xs)

buildASTIterate :: [TokorNode] -> [TokorNode]
buildASTIterate [] = []
buildASTIterate (l : ls) = case tryToMatch [] (T (TokenInfo TokError "")) (l : ls) of
  (A (ASTNodeError _), _) -> l : buildASTIterate ls
  (T (TokenInfo TokError _), []) -> l : buildASTIterate ls
  (something, xs) -> something : buildASTIterate xs

tryBuildInstructionList :: [TokorNode] -> ASTNode
tryBuildInstructionList [] = ASTNodeError (TokenInfo TokError "empty")
tryBuildInstructionList [A (ASTNodeParamList l)] = ASTNodeInstructionSequence l
tryBuildInstructionList [A (ASTNodeInstructionSequence l)] = ASTNodeInstructionSequence l
tryBuildInstructionList [A (ASTNodeInstructionSequence l), A n] = ASTNodeInstructionSequence (l ++ [n])
tryBuildInstructionList [A n1, A n2] = ASTNodeInstructionSequence [n1, n2]
tryBuildInstructionList l = ASTNodeError (TokenInfo TokError (show l))

expandParamLists :: [ASTNode] -> [ASTNode]
expandParamLists ((ASTNodeParamList l) : xs) = l ++ expandParamLists xs
expandParamLists (x : xs) = x : expandParamLists xs
expandParamLists [] = []

instructionSequenceExpandParamList :: ASTNode -> ASTNode
instructionSequenceExpandParamList (ASTNodeInstructionSequence l) = ASTNodeInstructionSequence (expandParamLists l)
instructionSequenceExpandParamList n = n

buildAST :: [TokorNode] -> ASTNode
buildAST [] = ASTNodeError (TokenInfo TokError "empty")
buildAST l = case buildASTIterate l of
  [A (ASTNodeParamList instr)] -> instructionSequenceExpandParamList (ASTNodeParamList instr)
  [A n] -> n
  ns ->
    if l == ns
      then
        tryBuildInstructionList ns
      else buildAST ns

containsParamList :: [ASTNode] -> Bool
containsParamList [] = False
containsParamList (ASTNodeParamList _ : _) = True
containsParamList (_ : xs) = containsParamList xs

check :: ASTNode -> ASTNode
check (ASTNodeParamList l) = check (ASTNodeInstructionSequence l)
check (ASTNodeInstructionSequence l) = if containsParamList l
  then check (ASTNodeInstructionSequence (expandParamLists l))
  else ASTNodeInstructionSequence l
check x = x

strToAST :: String -> ASTNode
strToAST str = check(instructionSequenceExpandParamList (buildAST (map T (tokenize str))))

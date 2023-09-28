module EvaluateAST
  ( instructionFromAST,
  )
where

import qualified Data.Maybe
import Lexer
    ( ASTNode(ASTNodeDefine, ASTNodeError, ASTNodeInteger,
              ASTNodeSymbol, ASTNodeSum, ASTNodeSub, ASTNodeMul, ASTNodeDiv,
              ASTNodeMod, astnsName) )
import VM (Context (..), Instruction (..), Param (..), Register (..), regGet, stackGetPointer, stackPush, symGet, symSet)

-- | Evaluates the AST and push the instructions into the context.
evaluateAST :: ASTNode -> Context -> Maybe Context
evaluateAST (ASTNodeError _) ctx = Nothing

instructionFromAST :: ASTNode -> Maybe Context -> Maybe Context
instructionFromAST _ Nothing = Nothing
instructionFromAST (ASTNodeInteger i) ctx = putIntegerInstruction (fromIntegral i) ctx
instructionFromAST (ASTNodeSymbol s) ctx = putSymbolInstruction s ctx
instructionFromAST (ASTNodeSum x) ctx = putSumInstruction x ctx
instructionFromAST (ASTNodeSub x) ctx = putSubInstruction x ctx
instructionFromAST (ASTNodeMul x) ctx = putMulInstruction x ctx
instructionFromAST (ASTNodeDiv x) ctx = putDivInstruction x ctx
instructionFromAST (ASTNodeMod x) ctx = putModInstruction x ctx
instructionFromAST (ASTNodeDefine name children) ctx = putDefineInstruction name children ctx
instructionFromAST _ _ = Nothing

putIntegerInstruction :: Int -> Maybe Context -> Maybe Context
putIntegerInstruction _ Nothing = Nothing
putIntegerInstruction i (Just ctx) = Just ctx {instructions = instructions ctx ++ [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate i)]}

putSumInstruction :: [ASTNode] -> Maybe Context -> Maybe Context
putSumInstruction _ Nothing = Nothing
putSumInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Just ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Add EAX (Reg EDI)]})
putSumInstruction _ _ = Nothing

putSubInstruction :: [ASTNode] -> Maybe Context -> Maybe Context
putSubInstruction _ Nothing = Nothing
putSubInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Just ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Sub (Reg EAX) (Reg EDI)]})
putSubInstruction _ _ = Nothing

putMulInstruction :: [ASTNode] -> Maybe Context -> Maybe Context
putMulInstruction _ Nothing = Nothing
putMulInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Just ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Mult (Reg EAX) (Reg EDI)]})
putMulInstruction _ _ = Nothing

putDivInstruction :: [ASTNode] -> Maybe Context -> Maybe Context
putDivInstruction _ Nothing = Nothing
putDivInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Just ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Div (Reg EAX) (Reg EDI)]})
putDivInstruction _ _ = Nothing

-- return (ctx'' {instructions = instructions ctx'' ++ [Mov (Reg EBX) (Reg EAX), Mov (Reg EAX) (Reg EDI), Mov (Reg EDI) (Reg EBX), Div (Reg EDI)]})

putModInstruction :: [ASTNode] -> Maybe Context -> Maybe Context
putModInstruction _ Nothing = Nothing
putModInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Just ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Mod (Reg EAX) (Reg EDI)]})
putModInstruction _ _ = Nothing

-- return (ctx'' {instructions = instructions ctx'' ++ [Mov (Reg EBX) (Reg EAX), Mov (Reg EAX) (Reg EDI), Mov (Reg EDI) (Reg EBX), Div (Reg EDI), Mov (Reg EAX) (Reg EDX)]})

putSymbolInstruction :: String -> Maybe Context -> Maybe Context
putSymbolInstruction _ Nothing = Nothing
putSymbolInstruction s (Just ctx) = do
  let sym = symGet (Just ctx) s
  case sym of
    Just sym' -> return (ctx {instructions = instructions ctx ++ [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate sym')]})
    Nothing -> Nothing

putDefineInstruction :: ASTNode -> [ASTNode] -> Maybe Context -> Maybe Context
putDefineInstruction _ _ Nothing = Nothing
putDefineInstruction name _ ctx = do
  let newCtx = instructionFromAST name ctx
  case newCtx of
    Just _ -> Nothing
    Nothing -> do
      eax <- regGet ctx EAX
      let ctx' = stackPush ctx eax
      let pointer = fst (stackGetPointer ctx')
      symSet ctx' (astnsName name) pointer

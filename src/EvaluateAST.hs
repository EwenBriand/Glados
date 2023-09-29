module EvaluateAST
  (instructionFromAST,
    astNodeArrayToHASM,
    strToHASM)
where

import qualified Data.Maybe
-- import Lexer
--     ( ASTNode(ASTNodeDefine, ASTNodeError, ASTNodeInteger,
--               ASTNodeSymbol, ASTNodeSum, ASTNodeSub, ASTNodeMul, ASTNodeDiv,
--               ASTNodeMod, astnsName, ASTNodeParamList, ASTNodeArray, strToAST) )
import Lexer
import VM (Context (..), Instruction (..), Param (..), Register (..), regGet, stackGetPointer, stackPush, symGet, symSet, labelSet, blockInitAllocVarSpace)


-- -- | Evaluates the AST and push the instructions into the context.
-- evaluateAST :: ASTNode -> Context -> Maybe Context
-- evaluateAST (ASTNodeError _) ctx = Nothing

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
instructionFromAST (ASTNodeParamList _) ctx = ctx -- not an actual instruction, does nothing
instructionFromAST (ASTNodeArray n) ctx = astNodeArrayToHASM ctx (ASTNodeArray n)

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
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Mov (Reg EBX) (Reg EAX), Mov (Reg EAX) (Reg EDI), Mov (Reg EDI) (Reg EBX), Div (Reg EDI)]})
putDivInstruction _ _ = Nothing

putModInstruction :: [ASTNode] -> Maybe Context -> Maybe Context
putModInstruction _ Nothing = Nothing
putModInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Just ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Mov (Reg EBX) (Reg EAX), Mov (Reg EAX) (Reg EDI), Mov (Reg EDI) (Reg EBX), Div (Reg EDI), Mov (Reg EAX) (Reg EDX)]})
putModInstruction _ _ = Nothing

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


-------------------------------------------------------------------------------
-- SOLVING CYCLE IMPORT
-------------------------------------------------------------------------------

astNodeArrayToHASMLoopBody :: Maybe Context -> [ASTNode] -> Maybe Context
astNodeArrayToHASMLoopBody Nothing _ = Nothing
astNodeArrayToHASMLoopBody (Just ctx) [] = Just ctx
astNodeArrayToHASMLoopBody (Just ctx) (x:xs) = case instructionFromAST x (Just ctx) of
    Nothing -> Nothing
    Just c -> astNodeArrayToHASMLoopBody (Just c {
        instructions = instructions c ++ [
            MovPtr (Reg ESI) (Reg EAX),       -- storing the value of the child node in the allocated memory
            Add ESI (Immediate 4)]}) xs      -- incrementing the pointer to the next element of the array

-- | @params:
--     ctx: the context to use
--     arr: the array to convert to HASM
astNodeArrayToHASM :: Maybe Context -> ASTNode -> Maybe Context
astNodeArrayToHASM Nothing _ = Nothing
astNodeArrayToHASM (Just ctx) (ASTNodeArray arr) = astNodeArrayToHASMEnd (astNodeArrayToHASMLoopBody (aSTNodeArrayToHASMPreLoop (Just ctx) arr) arr)
astNodeArrayToHASM _ _ = Nothing


hasmBackupRegisters :: [Register] -> [Instruction]
hasmBackupRegisters = foldr (\ x -> (++) [Push (Reg x)]) []

hasmRestoreRegisters :: [Register] -> [Instruction]
hasmRestoreRegisters = foldl (\ acc x -> acc ++ [Pop (Reg x)]) []

labelImpl :: Maybe Context -> String -> Int -> Maybe Context
labelImpl = labelSet

hASMPointerAlloc :: Int -> [Instruction]
hASMPointerAlloc size = [
    Mov (Reg EAX) (Immediate 0x2d),             -- syscall number for sbrk, malloc & puts ptr to eax after exec
    Mov (Reg EBX) (Immediate (size * 4)),       -- size of the array in ebx
    Interrupt,                        -- exec sbrk with int 0x80
    Mov (Reg EBX) (Reg EAX)]                    -- we put the pointer to the array in ebx we put the pointer to the array in ebx

aSTNodeArrayToHASMPreLoop :: Maybe Context -> [ASTNode] -> Maybe Context
aSTNodeArrayToHASMPreLoop Nothing _ = Nothing
aSTNodeArrayToHASMPreLoop (Just ctx) arr = Just ctx {instructions = instructions ctx ++
    hasmBackupRegisters [EBX, ESI] ++
    hASMPointerAlloc (length arr)
    ++ [Mov (Reg ESI) (Reg EBX)]}                    -- esi will be used to iterate over the array

astNodeArrayToHASMEnd :: Maybe Context -> Maybe Context
astNodeArrayToHASMEnd Nothing = Nothing
astNodeArrayToHASMEnd (Just ctx) = Just ctx {instructions = instructions ctx ++ [
    Mov (Reg EAX) (Reg EBX)]
    ++ hasmRestoreRegisters [EBX, ESI]} -- we put the pointer to the array in eax

strToHASM :: Maybe Context -> String -> Maybe Context
strToHASM Nothing _ = Nothing
strToHASM (Just ctx) str = c'
    where
        c' = case c of
            Nothing -> Nothing
            Just c2 -> Just c2 {instructions = blockInitAllocVarSpace (Just c2) ++ instructions c2}
        c = case strToAST str of
            ASTNodeError _ -> Nothing
            ast -> instructionFromAST ast (Just ctx)


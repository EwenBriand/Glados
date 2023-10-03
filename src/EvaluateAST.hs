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
-- import VM (Context (..), Instruction (..), Param (..), Register (..), regGet, stackGetPointer, stackPush, symGet, symSet, labelSet, blockInitAllocVarSpace, symGetTotalSize)
import VM


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
instructionFromAST (ASTNodeDefine name x) ctx = putDefineInstruction name x ctx
instructionFromAST (ASTNodeParamList _) ctx = ctx -- not an actual instruction, does nothing
instructionFromAST (ASTNodeArray n) ctx = astNodeArrayToHASM ctx (ASTNodeArray n)
instructionFromAST (ASTNodeInstructionSequence n) ctx = putInstructionSequence n ctx
instructionFromAST (ASTNodeBoolean b) ctx = putIntegerInstruction (if b then 1 else 0) ctx
instructionFromAST (ASTNodeIf cond thenBlock elseBlock) ctx = putIfInstruction ctx (ASTNodeIf cond thenBlock elseBlock)

instructionFromAST _ _ = Nothing

putInstructionSequence :: [ASTNode] -> Maybe Context -> Maybe Context
putInstructionSequence _ Nothing = Nothing
putInstructionSequence [] ctx = ctx
putInstructionSequence (x:xs) ctx = do
  ctx' <- instructionFromAST x ctx
  putInstructionSequence xs (Just ctx')

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
  ctx' <- instructionFromAST y ctx
  ctx'' <- instructionFromAST x (Just ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
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
    Just sym' -> return (ctx {instructions = instructions ctx ++ [Xor (Reg EAX) (Reg EAX), MovFromStackAddr (Reg EAX) (Immediate sym')]})
    Nothing -> Nothing

-- putDefineInstruction :: ASTNode -> ASTNode -> Maybe Context -> Maybe Context
-- putDefineInstruction _ _ Nothing = Nothing
-- putDefineInstruction name node ctx = do
--   let newCtx = instructionFromAST name ctx
--   case newCtx of
--     Just _ -> Nothing
--     Nothing -> do
--       let ctx' = symSet ctx (astnsName name) 4
--       let s = symGet ctx' (astnsName name)
--       let ctx'' = instructionFromAST node ctx'
--       case ctx'' of
--         Nothing -> Nothing
--         Just ctx''' ->  do
--           let res = symGetTotalSize ctx''
--           let res' = case res of
--                 Nothing -> 0
--                 Just res'' -> res''
--           case s of
--             Nothing -> Nothing
--             Just s' -> do
--               let val = res' - s'
--               return ( ctx''' {instructions = instructions ctx''' ++ [MovStackAddr (Immediate val) (Reg EAX)]})

inferTypeFromNode :: Maybe Context -> ASTNode -> VarType
inferTypeFromNode Nothing _ = GUndefinedType
inferTypeFromNode _ (ASTNodeInteger _) = GInt
inferTypeFromNode c (ASTNodeSymbol name) = case symGetFull c name of
    Nothing -> GUndefinedType
    Just (_, t) -> t
inferTypeFromNode c (ASTNodeSum (x:_)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeSub (x:_)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeMul (x:_)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeDiv (x:_)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeMod (x:_)) = inferTypeFromNode c x
inferTypeFromNode _ _ = GUndefinedType


putDefineNoErrCheck :: ASTNode -> ASTNode -> Maybe Context -> Maybe Context
putDefineNoErrCheck _ _ Nothing = Nothing
putDefineNoErrCheck name node c =
    let c' = symSet c (astnsName name) (inferTypeFromNode c node) in
        case instructionFromAST node c' of
        Nothing -> Nothing
        Just c'' -> Just c'' {instructions = instructions c'' ++ [MovStackAddr (Immediate (length (symTable (symbolTable c'')) - 1)) (Reg EAX)]}

putDefineInstruction :: ASTNode -> ASTNode -> Maybe Context -> Maybe Context
putDefineInstruction _ _ Nothing = Nothing
putDefineInstruction name node ctx =
    let newCtx = instructionFromAST name ctx in
        case newCtx of
        Just _ -> Nothing -- error, the variable already exists!
        Nothing -> putDefineNoErrCheck name node ctx

-- | Implements the following behaviour:
-- - tests the condition
-- stores the result in EAX
-- - if the condition is true, executes the then block and jumps after the else block
-- - if the condition is false, jumps to the else block and executes it
-- This is equivalent to the following nasm code:
-- cmp eax, 0
-- je <uuid>_else
-- <uuid>_then:
-- ...; executes the instructions in the then block
-- jmp <uuid>_end
-- <uuid>_else:
-- ...; executes the instructions of the children
-- <uuid>_end:
putIfInstruction :: Maybe Context -> ASTNode -> Maybe Context
putIfInstruction Nothing _ = Nothing
putIfInstruction (Just c) (ASTNodeIf cond thenBlock elseBlock) =
    let (uuid, c') = nextUUID c in do
        let c1 = ifPutCondition (Just c') cond
        let c2 = case c1 of
                Nothing -> Nothing
                Just c3 -> Just c3 { instructions = instructions c3 ++ [
                    Cmp (Reg EAX) (Immediate 0),
                    Je (show uuid ++ "else"),
                    Label (show uuid ++ "then") (length (instructions c3) + 3)]}
        let c3 = putInstructionSequence thenBlock c2
        let c4 = case c3 of
                Nothing -> Nothing
                Just c5 -> Just c5 { instructions = instructions c5 ++ [
                    Jmp (show uuid ++ "end"),
                    Label (show uuid ++ "else") (length (instructions c5) + 1)]}
        let c6 = case elseBlock of
                Nothing -> c4
                Just elseBlock' -> putInstructionSequence elseBlock' c4
        case c6 of
                Nothing -> Nothing
                Just c7 -> Just c7 { instructions = instructions c7 ++ [
                    Label (show uuid ++ "end") (length (instructions c7) + 1)]}
putIfInstruction _ _ = Nothing


ifPutCondition :: Maybe Context -> ASTNode -> Maybe Context
ifPutCondition Nothing _ = Nothing
ifPutCondition (Just c) cond = instructionFromAST cond (Just c)
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


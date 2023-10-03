module EvaluateAST
  (instructionFromAST,
    astNodeArrayToHASM,
    strToHASM)
where

-- import qualified Data.ValidState
-- import Lexer
--     ( ASTNode(ASTNodeDefine, ASTNodeError, ASTNodeInteger,
--               ASTNodeSymbol, ASTNodeSum, ASTNodeSub, ASTNodeMul, ASTNodeDiv,
--               ASTNodeMod, astnsName, ASTNodeParamList, ASTNodeArray, strToAST) )
import Lexer
-- import VM (Context (..), Instruction (..), Param (..), Register (..), regGet, stackGetPointer, stackPush, symGet, symSet, labelSet, blockInitAllocVarSpace, symGetTotalSize)
import VM
import ValidState


-- -- | Evaluates the AST and push the instructions into the context.
-- evaluateAST :: ASTNode -> Context -> ValidState Context
-- evaluateAST (ASTNodeError _) ctx = (Invalid "Error")

instructionFromAST :: ASTNode -> ValidState Context -> ValidState Context
instructionFromAST _ (Invalid s) = Invalid s
instructionFromAST (ASTNodeInteger i) ctx = putIntegerInstruction (fromIntegral i) ctx
instructionFromAST (ASTNodeSymbol s) ctx = putSymbolInstruction s ctx
instructionFromAST (ASTNodeSum x) ctx = putSumInstruction x ctx
instructionFromAST (ASTNodeSub x) ctx = putSubInstruction x ctx
instructionFromAST (ASTNodeMul x) ctx = putMulInstruction x ctx
instructionFromAST (ASTNodeDiv x) ctx = putDivInstruction x ctx
instructionFromAST (ASTNodeMod x) ctx = putModInstruction x ctx
instructionFromAST (ASTNodeDefine name x) ctx = putDefineInstruction name x ctx
instructionFromAST (ASTNodeParamList _) ctx = ctx -- not an actual instruction, does (Invalid "Error")
instructionFromAST (ASTNodeArray n) ctx = astNodeArrayToHASM ctx (ASTNodeArray n)
instructionFromAST (ASTNodeInstructionSequence n) ctx = putInstructionSequence n ctx
instructionFromAST (ASTNodeBoolean b) ctx = putIntegerInstruction (if b then 1 else 0) ctx
instructionFromAST (ASTNodeIf cond thenBlock elseBlock) ctx = putIfInstruction ctx (ASTNodeIf cond thenBlock elseBlock)

instructionFromAST _ _ = Invalid "Error"

putInstructionSequence :: [ASTNode] -> ValidState Context -> ValidState Context
putInstructionSequence _ (Invalid s) = Invalid s
putInstructionSequence [] ctx = ctx
putInstructionSequence (x:xs) ctx = do
  ctx' <- instructionFromAST x ctx
  putInstructionSequence xs (Valid ctx')

putIntegerInstruction :: Int -> ValidState Context -> ValidState Context
putIntegerInstruction _ (Invalid s) = Invalid s
putIntegerInstruction i (Valid ctx) = Valid ctx {instructions = instructions ctx ++ [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate i)]}

putSumInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putSumInstruction _ (Invalid s) = Invalid s
putSumInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Add EAX (Reg EDI)]})
putSumInstruction _ _ = Invalid "Error"

putSubInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putSubInstruction _ (Invalid s) = Invalid s
putSubInstruction [x, y] ctx = do
  ctx' <- instructionFromAST y ctx
  ctx'' <- instructionFromAST x (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Sub (Reg EAX) (Reg EDI)]})
putSubInstruction _ _ = Invalid "Error"

putMulInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putMulInstruction _ (Invalid "Error") = (Invalid "Error")
putMulInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Mult (Reg EAX) (Reg EDI)]})
putMulInstruction _ _ = Invalid "Error"

putDivInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putDivInstruction _ (Invalid s) = Invalid s
putDivInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Mov (Reg EBX) (Reg EAX), Mov (Reg EAX) (Reg EDI), Mov (Reg EDI) (Reg EBX), Div (Reg EDI)]})
putDivInstruction _ _ = Invalid "Error"

putModInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putModInstruction _ (Invalid s) = Invalid s
putModInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Mov (Reg EBX) (Reg EAX), Mov (Reg EAX) (Reg EDI), Mov (Reg EDI) (Reg EBX), Div (Reg EDI), Mov (Reg EAX) (Reg EDX)]})
putModInstruction _ _ = Invalid "Error"

putSymbolInstruction :: String -> ValidState Context -> ValidState Context
putSymbolInstruction _ (Invalid s) = Invalid s
putSymbolInstruction s (Valid ctx) = do
  let sym = symGet (Valid ctx) s
  case sym of
    Valid sym' -> return (ctx {instructions = instructions ctx ++ [Xor (Reg EAX) (Reg EAX), MovFromStackAddr (Reg EAX) (Immediate sym')]})
    Invalid s -> Invalid s

-- putDefineInstruction :: ASTNode -> ASTNode -> ValidState Context -> ValidState Context
-- putDefineInstruction _ _ (Invalid "Error") = (Invalid "Error")
-- putDefineInstruction name node ctx = do
--   let newCtx = instructionFromAST name ctx
--   case newCtx of
--     Valid _ -> (Invalid "Error")
--     (Invalid "Error") -> do
--       let ctx' = symSet ctx (astnsName name) 4
--       let s = symGet ctx' (astnsName name)
--       let ctx'' = instructionFromAST node ctx'
--       case ctx'' of
--         (Invalid "Error") -> (Invalid "Error")
--         Valid ctx''' ->  do
--           let res = symGetTotalSize ctx''
--           let res' = case res of
--                 (Invalid "Error") -> 0
--                 Valid res'' -> res''
--           case s of
--             (Invalid "Error") -> (Invalid "Error")
--             Valid s' -> do
--               let val = res' - s'
--               return ( ctx''' {instructions = instructions ctx''' ++ [MovStackAddr (Immediate val) (Reg EAX)]})

inferTypeFromNode :: ValidState Context -> ASTNode -> VarType
inferTypeFromNode (Invalid s) _ = GUndefinedType
inferTypeFromNode _ (ASTNodeInteger _) = GInt
inferTypeFromNode c (ASTNodeSymbol name) = case symGetFull c name of
    (Invalid _) -> GUndefinedType
    Valid (_, t) -> t
inferTypeFromNode c (ASTNodeSum (x:_)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeSub (x:_)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeMul (x:_)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeDiv (x:_)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeMod (x:_)) = inferTypeFromNode c x
inferTypeFromNode _ _ = GUndefinedType


putDefineNoErrCheck :: ASTNode -> ASTNode -> ValidState Context -> ValidState Context
putDefineNoErrCheck _ _ (Invalid s) = Invalid s
putDefineNoErrCheck name node c =
    let c' = symSet c (astnsName name) (inferTypeFromNode c node) in
        case instructionFromAST node c' of
        Invalid s -> Invalid s
        Valid c'' -> Valid c'' {instructions = instructions c'' ++ [MovStackAddr (Immediate (length (symTable (symbolTable c'')) - 1)) (Reg EAX)]}

putDefineInstruction :: ASTNode -> ASTNode -> ValidState Context -> ValidState Context
putDefineInstruction _ _ (Invalid s) = Invalid s
putDefineInstruction name node ctx =
    let newCtx = instructionFromAST name ctx in
        case newCtx of
        Valid _ -> Invalid "Error: Variable already exists" -- error, the variable already exists!
        Invalid _ -> putDefineNoErrCheck name node ctx

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
putIfInstruction :: ValidState Context -> ASTNode -> ValidState Context
putIfInstruction (Invalid s) _ = Invalid s
putIfInstruction (Valid c) (ASTNodeIf cond thenBlock elseBlock) =
    let (uuid, c') = nextUUID c in do
        let c1 = ifPutCondition (Valid c') cond
        let c2 = case c1 of
                (Invalid s) -> Invalid s
                Valid c3 -> Valid c3 { instructions = instructions c3 ++ [
                    Cmp (Reg EAX) (Immediate 0),
                    Je (show uuid ++ "else"),
                    Label (show uuid ++ "then") (length (instructions c3) + 3)]}
        let c3 = putInstructionSequence thenBlock c2
        let c4 = case c3 of
                (Invalid s) -> Invalid s
                Valid c5 -> Valid c5 { instructions = instructions c5 ++ [
                    Jmp (show uuid ++ "end"),
                    Label (show uuid ++ "else") (length (instructions c5) + 1)]}
        let c6 = case elseBlock of
                (Invalid s) -> c4
                Valid elseBlock' -> putInstructionSequence elseBlock' c4
        case c6 of
                (Invalid s) -> Invalid s
                Valid c7 -> Valid c7 { instructions = instructions c7 ++ [
                    Label (show uuid ++ "end") (length (instructions c7) + 1)]}
putIfInstruction _ _ = Invalid "Invalid arguments to if clause"

ifPutCondition :: ValidState Context -> ASTNode -> ValidState Context
ifPutCondition (Invalid s) _ = Invalid s
ifPutCondition (Valid c) cond = instructionFromAST cond (Valid c)
-------------------------------------------------------------------------------
-- SOLVING CYCLE IMPORT
-------------------------------------------------------------------------------

astNodeArrayToHASMLoopBody :: ValidState Context -> [ASTNode] -> ValidState Context
astNodeArrayToHASMLoopBody (Invalid s) _ = Invalid s
astNodeArrayToHASMLoopBody (Valid ctx) [] = Valid ctx
astNodeArrayToHASMLoopBody (Valid ctx) (x:xs) = case instructionFromAST x (Valid ctx) of
    (Invalid s) -> Invalid s
    Valid c -> astNodeArrayToHASMLoopBody (Valid c {
        instructions = instructions c ++ [
            MovPtr (Reg ESI) (Reg EAX),       -- storing the value of the child node in the allocated memory
            Add ESI (Immediate 4)]}) xs      -- incrementing the pointer to the next element of the array

-- | @params:
--     ctx: the context to use
--     arr: the array to convert to HASM
astNodeArrayToHASM :: ValidState Context -> ASTNode -> ValidState Context
astNodeArrayToHASM (Invalid s) _ = Invalid s
astNodeArrayToHASM (Valid ctx) (ASTNodeArray arr) = astNodeArrayToHASMEnd (astNodeArrayToHASMLoopBody (aSTNodeArrayToHASMPreLoop (Valid ctx) arr) arr)
astNodeArrayToHASM _ _ = Invalid "Error: could not resolve array"


hasmBackupRegisters :: [Register] -> [Instruction]
hasmBackupRegisters = foldr (\ x -> (++) [Push (Reg x)]) []

hasmRestoreRegisters :: [Register] -> [Instruction]
hasmRestoreRegisters = foldl (\ acc x -> acc ++ [Pop (Reg x)]) []

labelImpl :: ValidState Context -> String -> Int -> ValidState Context
labelImpl = labelSet

hASMPointerAlloc :: Int -> [Instruction]
hASMPointerAlloc size = [
    Mov (Reg EAX) (Immediate 0x2d),             -- syscall number for sbrk, malloc & puts ptr to eax after exec
    Mov (Reg EBX) (Immediate (size * 4)),       -- size of the array in ebx
    Interrupt,                        -- exec sbrk with int 0x80
    Mov (Reg EBX) (Reg EAX)]                    -- we put the pointer to the array in ebx we put the pointer to the array in ebx

aSTNodeArrayToHASMPreLoop :: ValidState Context -> [ASTNode] -> ValidState Context
aSTNodeArrayToHASMPreLoop (Invalid s) _ = Invalid s
aSTNodeArrayToHASMPreLoop (Valid ctx) arr = Valid ctx {instructions = instructions ctx ++
    hasmBackupRegisters [EBX, ESI] ++
    hASMPointerAlloc (length arr)
    ++ [Mov (Reg ESI) (Reg EBX)]}                    -- esi will be used to iterate over the array

astNodeArrayToHASMEnd :: ValidState Context -> ValidState Context
astNodeArrayToHASMEnd (Invalid s) = Invalid s
astNodeArrayToHASMEnd (Valid ctx) = Valid ctx {instructions = instructions ctx ++ [
    Mov (Reg EAX) (Reg EBX)]
    ++ hasmRestoreRegisters [EBX, ESI]} -- we put the pointer to the array in eax

strToHASM :: ValidState Context -> String -> ValidState Context
strToHASM (Invalid s) _ = Invalid s
strToHASM (Valid ctx) str = c'
    where
        c' = case c of
            (Invalid s) -> Invalid s
            Valid c2 -> Valid c2 {instructions = blockInitAllocVarSpace (Valid c2) ++ instructions c2}
        c = case strToAST str of
            ASTNodeError e -> Invalid ("Error: not a valid expression: " ++ show e)
            ast -> instructionFromAST ast (Valid ctx)


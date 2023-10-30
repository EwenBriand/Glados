module EvaluateAST
  ( instructionFromAST,
    astNodeArrayToHASM,
    strToHASM,
    putEqInstruction,
    putInferiorInstruction,
    putInferiorEqInstruction,
    putSuperiorInstruction,
    putSuperiorEqInstruction,
    putNotEqualInstruction,
    putMutableInstruction,
    putPrintInstruction,
    putBoolInstruction,
    putDefineInstruction,
    putWhileInstruction,
    putSetInstruction,
    paramsRegisters,
    evalParamsToReg,
    putFunctionCall,
    pushParamTypeToBlock,
    evaluateBlock,
    evaluateBlockOneInstr,
    copyParentBlocks,
    declSymbolBlock,
    setupBlockParams,
    putSumInstruction,
    putSubInstruction,
    putMulInstruction,
    putDivInstruction,
    putModInstruction,
    tryPutFunctionCall,
    putSymbolInstruction,
    putWhileCondition,
    inferTypeFromNode,
    putMutableNoErrCheck,
    putSetNoErrCheck,
    putIfInstruction,
    ifPutCondition,
    astNodeArrayToHASMLoopBody,
    hasmBackupRegisters,
    hasmRestoreRegisters,
    labelImpl,
    hASMPointerAlloc,
    aSTNodeArrayToHASMPreLoop,
    astNodeArrayToHASMEnd,
    putIntegerInstruction,
    putInstructionSequence
  )
where

-- import qualified Data.ValidState
-- import Lexer
--     ( ASTNode(ASTNodeMutable, ASTNodeError, ASTNodeInteger,
--               ASTNodeSymbol, ASTNodeSum, ASTNodeSub, ASTNodeMul, ASTNodeDiv,
--               ASTNodeMod, astnsName, ASTNodeParamList, ASTNodeArray, strToAST) )

-- import VM (Context (..), Instruction (..), Param (..), Register (..), regGet, stackGetPointer, stackPush, symGet, symSet, labelSet, blockInitAllocVarSpace, symGetTotalSize)

import Lexer
import VM
import VM (Context (Context))
import ValidState

-- -- | Evaluates the AST and push the instructions into the context.
-- evaluateAST :: ASTNode -> Context -> ValidState Context
-- evaluateAST (ASTNodeError _) ctx = (Invalid "Error")

instructionFromAST :: ASTNode -> ValidState Context -> ValidState Context
instructionFromAST _ (Invalid s) = Invalid s
instructionFromAST (ASTNodeIf (ASTNodeArray cond) thenBlock elseBlock) ctx = putIfInstruction ctx (ASTNodeIf (head cond) thenBlock elseBlock)
instructionFromAST (ASTNodeIf cond [ASTNodeParamList thenBlock] (Valid [ASTNodeParamList elseBlock])) ctx = putIfInstruction ctx (ASTNodeIf cond (expendParamList thenBlock) (Valid (expendParamList elseBlock)))
instructionFromAST (ASTNodeIf cond [ASTNodeParamList thenBlock] elseBlock) ctx = putIfInstruction ctx (ASTNodeIf cond (expendParamList thenBlock) elseBlock)
instructionFromAST (ASTNodeIf cond thenBlock (Valid [ASTNodeParamList elseBlock])) ctx = putIfInstruction ctx (ASTNodeIf cond thenBlock (Valid (expendParamList elseBlock)))
instructionFromAST (ASTNodeIf cond thenBlock elseBlock) ctx = putIfInstruction ctx (ASTNodeIf cond thenBlock elseBlock)
instructionFromAST (ASTNodeElif cond [ASTNodeParamList thenBlock] (Valid [ASTNodeParamList elseBlock])) ctx = putIfInstruction ctx (ASTNodeIf cond (expendParamList thenBlock) (Valid (expendParamList elseBlock)))
instructionFromAST (ASTNodeElif cond [ASTNodeParamList thenBlock] elseBlock) ctx = putIfInstruction ctx (ASTNodeIf cond (expendParamList thenBlock) elseBlock)
instructionFromAST (ASTNodeElif cond thenBlock (Valid [ASTNodeParamList elseBlock])) ctx = putIfInstruction ctx (ASTNodeIf cond thenBlock (Valid (expendParamList elseBlock)))
instructionFromAST (ASTNodeElif cond thenBlock elseBlock) ctx = putIfInstruction ctx (ASTNodeIf cond thenBlock elseBlock)
instructionFromAST (ASTNodeDefine name params [ASTNodeParamList body]) ctx = putDefineInstruction ctx name params [ASTNodeInstructionSequence (expendParamList body)]
instructionFromAST (ASTNodeDefine name params body) c = putDefineInstruction c name params body
instructionFromAST (ASTNodeInteger i) ctx = putIntegerInstruction (fromIntegral i) ctx
instructionFromAST (ASTNodeSymbol s) ctx = putSymbolInstruction s ctx
instructionFromAST (ASTNodeSum x) ctx = putSumInstruction x ctx
instructionFromAST (ASTNodeSub x) ctx = putSubInstruction x ctx
instructionFromAST (ASTNodeMul x) ctx = putMulInstruction x ctx
instructionFromAST (ASTNodeDiv x) ctx = putDivInstruction x ctx
instructionFromAST (ASTNodeMod x) ctx = putModInstruction x ctx
instructionFromAST (ASTNodeEq x) ctx = putEqInstruction x ctx
instructionFromAST (ASTNodeInferior x) ctx = putInferiorInstruction x ctx
instructionFromAST (ASTNodeInferiorEq x) ctx = putInferiorEqInstruction x ctx
instructionFromAST (ASTNodeSuperior x) ctx = putSuperiorInstruction x ctx
instructionFromAST (ASTNodeSuperiorEq x) ctx = putSuperiorEqInstruction x ctx
instructionFromAST (ASTNodeNotEqual x) ctx = putNotEqualInstruction x ctx
instructionFromAST (ASTNodeMutable (ASTNodeType symtyp) name (ASTNodeType symval) x) ctx = putMutableInstruction symtyp name symval x ctx
instructionFromAST (ASTNodeParamList n) ctx = ctx
instructionFromAST (ASTNodeArray n) ctx = astNodeArrayToHASM ctx (ASTNodeArray n)
instructionFromAST (ASTNodeInstructionSequence n) ctx = putInstructionSequence n ctx
instructionFromAST (ASTNodePrint n) ctx = putPrintInstruction ctx n
instructionFromAST (ASTNodeBoolean b) ctx = putBoolInstruction (if b then 1 else 0) ctx
instructionFromAST (ASTNodeFunctionCall name params) ctx = putFunctionCall ctx name params
instructionFromAST (ASTNodeLambda name params body) ctx = putDefineInstruction ctx name params body
instructionFromAST (ASTNodeWhile cond [(ASTNodeInstructionSequence [(ASTNodeParamList body), next])]) ctx = putWhileInstruction ctx cond ((expendParamList body) ++ [next])
instructionFromAST (ASTNodeWhile cond [(ASTNodeParamList body)]) ctx = putWhileInstruction ctx cond (expendParamList body)
instructionFromAST (ASTNodeWhile cond body) ctx = putWhileInstruction ctx cond body
instructionFromAST (ASTNodeSet name value) ctx = putSetInstruction ctx name value
instructionFromAST (ASTNodeReturn value) ctx = putReturnInstruction ctx value
instructionFromAST (ASTNodeBreak [ASTNodeLambda _ param body, ASTNodeFunctionCall _ params]) ctx = instructionFromAST (ASTNodeBreak [(ASTNodeFunctionCall u_name params)]) (instructionFromAST (ASTNodeLambda (ASTNodeSymbol u_name) param body) (Valid ctx'))
  where
    u_name = "lambda@" ++ ("_" ++ show uuid)
    (uuid, ctx') = nextUUIDValid ctx
instructionFromAST (ASTNodeBreak (a : b)) ctx = instructionFromAST (ASTNodeBreak b) (instructionFromAST a ctx)
instructionFromAST (ASTNodeBreak []) ctx = ctx
instructionFromAST a _ = Invalid ("Error: invalid AST" ++ show a)

paramsRegisters :: [Register]
paramsRegisters = [EDI, ESI, EDX, ECX]

evalParamsToReg :: ValidState Context -> [ASTNode] -> [Register] -> ValidState Context
evalParamsToReg (Invalid s) _ _ = Invalid s
evalParamsToReg (Valid c) [] _ = Valid c
evalParamsToReg _ _ [] = Invalid "Error: too many parameters (max: 4)"
evalParamsToReg (Valid c) (p : ps) (r : rs) = case instructionFromAST p (Valid c) of
  Invalid s -> Invalid s
  Valid c' -> evalParamsToReg (Valid c' {instructions = instructions c' ++ [Mov (Reg r) (Reg EAX)]}) ps rs

putFunctionCall :: ValidState Context -> String -> [ASTNode] -> ValidState Context
putFunctionCall (Invalid s) _ _ = Invalid s
putFunctionCall (Valid c) name params = case evalParamsToReg (Valid c) params paramsRegisters of
  Invalid s -> Invalid s
  Valid c' -> tryPutFunctionCall (Valid c') name

pushParamTypeToBlock :: ValidState Block -> [ASTNode] -> ValidState Block
pushParamTypeToBlock (Invalid s) _ = Invalid s
pushParamTypeToBlock blk [] = blk
pushParamTypeToBlock (Valid blk) (x : xs) =
  pushParamTypeToBlock
    (Valid (blk {blockParamTypes = blockParamTypes blk ++ [inferTypeFromNode (blockContext blk) x]}))
    xs

declSymbolBlock :: Block -> [ASTNode] -> ValidState Block
declSymbolBlock blk [] = Valid blk
declSymbolBlock blk (ASTNodeVariable  (ASTNodeSymbol paramName) typ : ps) = case symSet (blockContext blk) paramName typ of -- Todo set types here
  Invalid s -> Invalid ("While declaring parameter: \n\t" ++ s)
  Valid ctx -> declSymbolBlock (blk {blockContext = Valid ctx}) ps
declSymbolBlock blk (ASTNodeSymbol paramName : ps) = case symSet (blockContext blk) paramName (GUndefinedType) of -- Todo set types here
  Invalid s -> Invalid ("While declaring parameter: \n\t" ++ s)
  Valid ctx -> declSymbolBlock (blk {blockContext = Valid ctx}) ps
declSymbolBlock _ _ = Invalid "Error: invalid parameter: expected symbol"

setupBlockParams :: Block -> ValidState ASTNode -> ValidState Block
setupBlockParams blk (Invalid _) = Valid blk
setupBlockParams blk (Valid (ASTNodeParamList n)) = pushParamTypeToBlock (declSymbolBlock blk n) n
setupBlockParams blk _ = Valid blk

evaluateBlock :: ValidState Context -> Block -> ValidState ASTNode -> [ASTNode] -> ValidState Context
evaluateBlock (Invalid s) _ _ _ = Invalid s
evaluateBlock (Valid c) _ _ [] = Valid c
evaluateBlock (Valid c) blk params (x : xs) = case evaluateBlockOneInstr (Valid c) blk params x of
  Invalid s -> Invalid s
  Valid c' -> evaluateBlock (Valid c') blk params xs

evaluateBlockOneInstr :: ValidState Context -> Block -> ValidState ASTNode -> ASTNode -> ValidState Context
evaluateBlockOneInstr (Invalid s) _ _ _ = Invalid s
evaluateBlockOneInstr (Valid c) blk _ body = case instructionFromAST body (blockContext blk) of
  Invalid s -> Invalid s
  Valid c' -> blockReplace (Valid c) (Valid blk {blockContext = Valid c'})

copyParentBlocks :: Context -> Block -> Block
copyParentBlocks ctx blk = case blockContext blk of
  Invalid _ -> blk
  Valid ctx' -> blk {blockContext = Valid ctx' {blocks = blocks ctx}}

putDefineInstruction :: ValidState Context -> ASTNode -> ValidState ASTNode -> [ASTNode] -> ValidState Context
putDefineInstruction (Invalid s) _ _ _ = Invalid ("While defining function: \n\t" ++ s)
putDefineInstruction (Valid c) name params [ASTNodeArray body] = putDefineInstruction (Valid c) name params body
putDefineInstruction (Valid c) name params body = case blockAdd (Valid c) (astnsName name) of
  Invalid s -> Invalid s
  Valid ctx -> case blockGet (Valid ctx) (astnsName name) of
    Invalid s -> Invalid s
    Valid blk -> case setupBlockParams blk params of
      Invalid s -> Invalid ("While parsing the parameters of the function: \n" ++ s)
      Valid blk' -> evaluateBlock (Valid ctx) (copyParentBlocks ctx blk') params body

putParamListInstruction :: ValidState Context -> [ASTNode] -> ValidState Context
putParamListInstruction (Invalid s) _ = Invalid s
putParamListInstruction ctx [] = ctx
putParamListInstruction ctx x = instructionFromAST (ASTNodeInstructionSequence x) ctx

-- 4 is the syscall for out in asm
putPrintInstruction :: ValidState Context -> ASTNode -> ValidState Context
putPrintInstruction (Invalid s) _ = Invalid s
putPrintInstruction ctx node = do
  ctx' <- instructionFromAST node ctx
  Valid ctx' {instructions = instructions ctx' ++ [Xor (Reg EBX) (Reg EBX), Mov (Reg EBX) (Immediate (typeToInt (inferTypeFromNode (Valid ctx') node))), Xor (Reg ECX) (Reg ECX), Mov (Reg ECX) (Reg EAX), Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 4), Interrupt]}

-- 5 is for 4 (numeric) + 1
-- putPrintInstruction (Valid ctx) = Valid ctx {instructions = instructions ctx ++ [Mov (Reg EAX) 4, Mov (Reg EBX) 1, Mov (Reg ECX) (Reg ), Mov (Reg EDX) 5, Interrupt]}

putInstructionSequence :: [ASTNode] -> ValidState Context -> ValidState Context
putInstructionSequence _ (Invalid s) = Invalid s
putInstructionSequence [] ctx = ctx
putInstructionSequence (x : xs) ctx = do
  ctx' <- instructionFromAST x ctx
  putInstructionSequence xs (Valid ctx')

putIntegerInstruction :: Int -> ValidState Context -> ValidState Context
putIntegerInstruction _ (Invalid s) = Invalid s
putIntegerInstruction i (Valid ctx) = Valid ctx {instructions = instructions ctx ++ [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate i)]}

putBoolInstruction :: Int -> ValidState Context -> ValidState Context
putBoolInstruction _ (Invalid s) = Invalid s
putBoolInstruction i (Valid ctx) = Valid ctx {instructions = instructions ctx ++ [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate i), Cmp (Reg EAX) (Immediate 1)]}

putSumInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putSumInstruction _ (Invalid s) = Invalid s
putSumInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Add EAX (Reg EDI)]})
putSumInstruction _ _ = Invalid "Error"

putEqInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putEqInstruction _ (Invalid s) = Invalid s
putEqInstruction [x, y] (Valid ctx) = do
  ctx' <- instructionFromAST x (Valid c)
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Cmp (Reg EDI) (Reg EAX), Je (("_" ++ show uuid) ++ "eq"), Mov (Reg EAX) (Immediate 0), Jmp (("_" ++ show uuid) ++ "eqend"), Label (("_" ++ show uuid) ++ "eq") (length (instructions ctx'') + 1), Mov (Reg EAX) (Immediate 1), Label (("_" ++ show uuid) ++ "eqend") (length (instructions ctx'') + 1)]})
  where
    (uuid, c) = nextUUID ctx
putEqInstruction _ _ = Invalid "Error"

putNotEqualInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putNotEqualInstruction _ (Invalid s) = Invalid s
putNotEqualInstruction [x, y] (Valid ctx) = do
  ctx' <- instructionFromAST x (Valid c)
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Cmp (Reg EDI) (Reg EAX), Jne (show uuid ++ "neq"), Mov (Reg EAX) (Immediate 0), Jmp (show uuid ++ "neqend"), Label (show uuid ++ "neq") (length (instructions ctx'') + 1), Mov (Reg EAX) (Immediate 1), Label (show uuid ++ "neqend") (length (instructions ctx'') + 1)]})
  where
    (uuid, c) = nextUUID ctx

putInferiorInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putInferiorInstruction _ (Invalid s) = Invalid s
putInferiorInstruction [x, y] (Valid ctx) = do
  ctx' <- instructionFromAST x (Valid c)
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Cmp (Reg EDI) (Reg EAX), Jl (("_" ++ show uuid) ++ "inf"), Mov (Reg EAX) (Immediate 0), Jmp (("_" ++ show uuid) ++ "infend"), Label (("_" ++ show uuid) ++ "inf") (length (instructions ctx'') + 1), Mov (Reg EAX) (Immediate 1), Label (("_" ++ show uuid) ++ "infend") (length (instructions ctx'') + 1)]})
  where
    (uuid, c) = nextUUID ctx
putInferiorInstruction _ _ = Invalid "Error"

putInferiorEqInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putInferiorEqInstruction _ (Invalid s) = Invalid s
putInferiorEqInstruction [x, y] (Valid ctx) = do
  ctx' <- instructionFromAST x (Valid c)
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Cmp (Reg EDI) (Reg EAX), Jle (("_" ++ show uuid) ++ "inf"), Mov (Reg EAX) (Immediate 0), Jmp (("_" ++ show uuid) ++ "infeqend"), Label (("_" ++ show uuid) ++ "inf") (length (instructions ctx'') + 1), Mov (Reg EAX) (Immediate 1), Label (("_" ++ show uuid) ++ "infeqend") (length (instructions ctx'') + 1)]})
  where
    (uuid, c) = nextUUID ctx
putInferiorEqInstruction _ _ = Invalid "Error"

putSuperiorEqInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putSuperiorEqInstruction _ (Invalid s) = Invalid s
putSuperiorEqInstruction [x, y] (Valid ctx) = do
  ctx' <- instructionFromAST x (Valid c)
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Cmp (Reg EDI) (Reg EAX), Jge (("_" ++ show uuid) ++ "inf"), Mov (Reg EAX) (Immediate 0), Jmp (("_" ++ show uuid) ++ "supeqend"), Label (("_" ++ show uuid) ++ "inf") (length (instructions ctx'') + 1), Mov (Reg EAX) (Immediate 1), Label (("_" ++ show uuid) ++ "supeqend") (length (instructions ctx'') + 1)]})
  where
    (uuid, c) = nextUUID ctx
putSuperiorEqInstruction _ _ = Invalid "Error"

putSuperiorInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putSuperiorInstruction _ (Invalid s) = Invalid s
putSuperiorInstruction [x, y] (Valid ctx) = do
  ctx' <- instructionFromAST x (Valid c)
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Cmp (Reg EDI) (Reg EAX), Jg (("_" ++ show uuid) ++ "inf"), Mov (Reg EAX) (Immediate 0), Jmp (("_" ++ show uuid) ++ "supend"), Label (("_" ++ show uuid) ++ "inf") (length (instructions ctx'') + 1), Mov (Reg EAX) (Immediate 1), Label (("_" ++ show uuid) ++ "supend") (length (instructions ctx'') + 1)]})
  where
    (uuid, c) = nextUUID ctx
putSuperiorInstruction _ _ = Invalid "Error"

putSubInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putSubInstruction _ (Invalid s) = Invalid s
putSubInstruction [x, y] ctx = do
  ctx' <- instructionFromAST y ctx
  ctx'' <- instructionFromAST x (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Sub (Reg EAX) (Reg EDI)]})
putSubInstruction _ _ = Invalid "Error"

putMulInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putMulInstruction _ (Invalid "Error") = (Invalid "Error")
putMulInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Mult (Reg EAX) (Reg EDI)]})
putMulInstruction _ _ = Invalid "Error"

putDivInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putDivInstruction _ (Invalid s) = Invalid s
putDivInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Mov (Reg EBX) (Reg EAX), Mov (Reg EAX) (Reg EDI), Mov (Reg EDI) (Reg EBX), Div (Reg EDI)]})
putDivInstruction _ _ = Invalid "Error"

putModInstruction :: [ASTNode] -> ValidState Context -> ValidState Context
putModInstruction _ (Invalid s) = Invalid s
putModInstruction [x, y] ctx = do
  ctx' <- instructionFromAST x ctx
  ctx'' <- instructionFromAST y (Valid ctx' {instructions = instructions ctx' ++ [Push (Reg EAX)]})
  Prelude.return (ctx'' {instructions = instructions ctx'' ++ [Pop (Reg EDI), Mov (Reg EBX) (Reg EAX), Mov (Reg EAX) (Reg EDI), Mov (Reg EDI) (Reg EBX), Div (Reg EDI), Mov (Reg EAX) (Reg EDX)]})
putModInstruction _ _ = Invalid "Error"

tryPutFunctionCall :: ValidState Context -> String -> ValidState Context
tryPutFunctionCall (Invalid s) _ = Invalid s
tryPutFunctionCall (Valid ctx) s = case blockGet (Valid ctx) s of
  (Invalid _) -> Invalid ("Symbol or Function not found: " ++ s)
  Valid _ -> Prelude.return ctx {instructions = instructions ctx ++ [Call s]}

putSymbolInstruction :: String -> ValidState Context -> ValidState Context
putSymbolInstruction _ (Invalid s) = Invalid s
putSymbolInstruction s (Valid ctx) = do
  let sym = symGet (Valid ctx) s
  case sym of
    Valid sym' -> Prelude.return (ctx {instructions = instructions ctx ++ [Xor (Reg EAX) (Reg EAX), MovFromStackAddr (Reg EAX) (Immediate sym')]})
    Invalid _ -> tryPutFunctionCall (Valid ctx) s

putWhileInstruction :: ValidState Context -> ASTNode -> [ASTNode] -> ValidState Context
putWhileInstruction (Invalid s) _ _ = Invalid s
putWhileInstruction (Valid ctx) cond body = do
  let (uuid, ctx') = nextUUID ctx
  let ctx'' = putWhileCondition (Valid ctx'{instructions = instructions ctx' ++ [Label (show uuid ++ "while") (length (instructions ctx') + 1)]}) cond uuid
  let ctx''' = putInstructionSequence body ctx''
  case ctx''' of
    (Invalid s) -> Invalid s
    Valid ctx'''' ->
      Valid
        ctx''''
          { instructions =
              instructions ctx''''
                ++ [ Jmp (show uuid ++ "while"),
                     Label (show uuid ++ "end") (length (instructions ctx'''') + 1)
                   ]
          }

putWhileCondition :: ValidState Context -> ASTNode -> Int -> ValidState Context
putWhileCondition (Invalid s) _ _ = Invalid s
putWhileCondition ctx cond uuid = do
  let ctx' = instructionFromAST cond ctx
  case ctx' of
    (Invalid s) -> Invalid s
    Valid ctx'' ->
      Valid
        ctx''
          { instructions =
              instructions ctx''
                ++ [ Cmp (Reg EAX) (Immediate 1),
                     Jne (show uuid ++ "end")
                   ]
          }

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
--               Prelude.return ( ctx''' {instructions = instructions ctx''' ++ [MovStackAddr (Immediate val) (Reg EAX)]})

inferTypeFromNode :: ValidState Context -> ASTNode -> VarType
inferTypeFromNode (Invalid _) _ = GUndefinedType
inferTypeFromNode _ (ASTNodeInteger _) = GInt
inferTypeFromNode _ (ASTNodeBoolean _) = GBool
inferTypeFromNode c (ASTNodeSymbol name) = case symGetFull c name of
  (Invalid _) -> GUndefinedType
  Valid (_, t) -> t
inferTypeFromNode c (ASTNodeSum (x : _)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeSub (x : _)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeMul (x : _)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeDiv (x : _)) = inferTypeFromNode c x
inferTypeFromNode c (ASTNodeMod (x : _)) = inferTypeFromNode c x
inferTypeFromNode _ _ = GUndefinedType

putMutableNoErrCheck :: VarType -> ASTNode -> ASTNode -> ValidState Context -> ValidState Context
putMutableNoErrCheck _ _ _ (Invalid s) = Invalid s
putMutableNoErrCheck symtyp name node c =
  let c' = symSet c (astnsName name) (inferTypeFromNode c node)
   in case instructionFromAST node c' of
        Invalid s -> Invalid s
        Valid c'' -> Valid c'' {instructions = instructions c'' ++ [Push (Immediate 0), MovStackAddr (Immediate (length (symTable (symbolTable c'')) - 1)) (Reg EAX)]}

putMutableInstruction :: VarType -> ASTNode -> VarType -> ASTNode -> ValidState Context -> ValidState Context
putMutableInstruction _ _ _ _ (Invalid s) = Invalid s
putMutableInstruction symtyp name valtyp node ctx =
  let newCtx = instructionFromAST name ctx
   in case newCtx of
        Valid _ -> Invalid "Error: Variable already exists" -- error, the variable already exists!
        -- Invalid _ -> putMutableNoErrCheck symtyp name vartyp node ctx
        Invalid _ -> if symtyp == valtyp then putMutableNoErrCheck symtyp name node ctx else Invalid "Error: type mismatch"

putSetNoErrCheck :: ValidState Context -> ASTNode -> ASTNode -> ValidState Context
putSetNoErrCheck (Invalid s) _ _ = Invalid s
putSetNoErrCheck ctx name node = do
  let ctx' = instructionFromAST node ctx
    in case ctx' of
      Invalid s -> Invalid s
      Valid ctx' -> (Valid ctx' {instructions = instructions ctx' ++ [MovStackAddr (Immediate addr) (Reg EAX)]})
      where
        addr = case symGet ctx (astnsName name) of
          Invalid _ -> 0
          Valid addr' -> addr'


putSetInstruction :: ValidState Context -> ASTNode -> ASTNode -> ValidState Context
putSetInstruction (Invalid s) _ _ = Invalid s
putSetInstruction ctx name node =
  let newCtx = instructionFromAST name ctx
    in case newCtx of
      Invalid _ -> Invalid "Error: Variable does't exists" -- error, the variable
      Valid _ -> putSetNoErrCheck ctx name node

putReturnInstruction :: ValidState Context -> ASTNode -> ValidState Context
putReturnInstruction (Invalid s) _ = Invalid s
putReturnInstruction ctx node = do
  let ctx' = instructionFromAST node ctx
  case ctx' of
    Invalid s -> Invalid s
    Valid ctx' -> (Valid ctx' {instructions = instructions ctx' ++ [Ret]})

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
  let (uuid, c') = nextUUID c
   in do
        let c2 = ifPutCondition (Valid c') cond uuid
        let c3 = putInstructionSequence thenBlock c2
        let c4 = case c3 of
              (Invalid s) -> Invalid s
              Valid c5 ->
                Valid
                  c5
                    { instructions =
                        instructions c5
                          ++ [ Jmp (("_" ++ show uuid) ++ "end"),
                               Label (("_" ++ show uuid) ++ "else") (length (instructions c5) + 1)
                             ]
                    }
        let c6 = case elseBlock of
              (Invalid _) -> c4
              Valid elseBlock' -> putInstructionSequence elseBlock' c4
        case c6 of
          (Invalid s) -> Invalid s
          Valid c7 ->
            Valid
              c7
                { instructions =
                    instructions c7
                      ++ [ Label (("_" ++ show uuid) ++ "end") (length (instructions c7) + 1)
                         ]
                }
putIfInstruction _ _ = Invalid "Invalid arguments to if clause"

ifPutCondition :: ValidState Context -> ASTNode -> Int -> ValidState Context
ifPutCondition (Invalid s) _ _ = Invalid s
ifPutCondition c cond uuid = do
  case instructionFromAST cond c of
    (Invalid s) -> Invalid s
    Valid c' ->
      Valid
        c'
          { instructions =
              instructions c'
                ++ [ Cmp (Reg EAX) (Immediate 1),
                     Jne (("_" ++ show uuid) ++ "else"),
                     Label (("_" ++ show uuid) ++ "then") (length (instructions c') + 3)
                   ]
          }

-------------------------------------------------------------------------------
-- SOLVING CYCLE IMPORT
-------------------------------------------------------------------------------

astNodeArrayToHASMLoopBody :: ValidState Context -> [ASTNode] -> ValidState Context
astNodeArrayToHASMLoopBody (Invalid s) _ = Invalid s
astNodeArrayToHASMLoopBody (Valid ctx) [] = Valid ctx
astNodeArrayToHASMLoopBody (Valid ctx) (x : xs) = case instructionFromAST x (Valid ctx) of
  (Invalid s) -> Invalid s
  Valid c ->
    astNodeArrayToHASMLoopBody
      ( Valid
          c
            { instructions =
                instructions c
                  ++ [ MovPtr (Reg ESI) (Reg EAX), -- storing the value of the child node in the allocated memory
                       Add ESI (Immediate 1)
                     ]
            }
      )
      xs -- incrementing the pointer to the next element of the array

-- | @params:
--     ctx: the context to use
--     arr: the array to convert to HASM
astNodeArrayToHASM :: ValidState Context -> ASTNode -> ValidState Context
astNodeArrayToHASM (Invalid s) _ = Invalid s
astNodeArrayToHASM ctx (ASTNodeArray []) = ctx
astNodeArrayToHASM (Valid ctx) (ASTNodeArray arr) = astNodeArrayToHASMEnd (astNodeArrayToHASMLoopBody (aSTNodeArrayToHASMPreLoop (Valid ctx) arr) arr)
astNodeArrayToHASM _ _ = Invalid "Error: could not resolve array"

hasmBackupRegisters :: [Register] -> [Instruction]
hasmBackupRegisters = foldr (\x -> (++) [Push (Reg x)]) []

hasmRestoreRegisters :: [Register] -> [Instruction]
hasmRestoreRegisters = foldl (\acc x -> acc ++ [Pop (Reg x)]) []

labelImpl :: ValidState Context -> String -> Int -> ValidState Context
labelImpl = labelSet

hASMPointerAlloc :: Int -> [Instruction]
hASMPointerAlloc size =
  [ Mov (Reg EAX) (Immediate 0x2d), -- syscall number for sbrk, malloc & puts ptr to eax after exec
    Alloc size, -- size of the array in ebx
    Mov (Reg EBX) (Reg EAX) -- we put the pointer to the array in ebx
  ]

aSTNodeArrayToHASMPreLoop :: ValidState Context -> [ASTNode] -> ValidState Context
aSTNodeArrayToHASMPreLoop (Invalid s) _ = Invalid s
aSTNodeArrayToHASMPreLoop (Valid ctx) arr =
  Valid
    ctx
      { instructions =
          instructions ctx
            ++ hasmBackupRegisters [EBX, ESI]
            ++ hASMPointerAlloc (length arr)
            ++ [Mov (Reg ESI) (Reg EBX)] -- esi will be used to iterate over the array
      }

astNodeArrayToHASMEnd :: ValidState Context -> ValidState Context
astNodeArrayToHASMEnd (Invalid s) = Invalid s
astNodeArrayToHASMEnd (Valid ctx) =
  Valid
    ctx
      { instructions =
          instructions ctx
            ++ [ Mov (Reg EAX) (Reg EBX)
               ]
            ++ hasmRestoreRegisters [EBX, ESI] -- we put the pointer to the array in eax
      }

strToHASM :: ValidState Context -> String -> ValidState Context
strToHASM (Invalid s) _ = Invalid s
strToHASM (Valid ctx) str = c'
  where
    c' = case c of
      (Invalid s) -> Invalid s
      -- Valid c2 -> Valid c2 {instructions = blockInitAllocVarSpace (Valid c2) ++ instructions c2}
      Valid c2 -> Valid c2 {instructions = [Enter] ++ instructions c2}
    c = case strToAST str of
      ASTNodeError e -> Invalid ("Error: not a valid expression: " ++ show e)
      ast -> instructionFromAST ast (Valid ctx {cAST = [ast]})

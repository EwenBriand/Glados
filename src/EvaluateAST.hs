module EvaluateAST (
    instructionFromAST
) where

import MyLexer
import VM(Param(..), Instruction(..), Context(..), Register(..), symGet, regGet, stackPush, stackGetPointer, symSet)

-- | Evaluates the AST and push the instructions into the context.

evaluateAST :: ASTNode -> Context -> Maybe Context
evaluateAST (ASTNodeError _) ctx = Nothing


instructionFromAST :: ASTNode -> Context -> Maybe Context
instructionFromAST (ASTNodeInteger i) ctx = Just (putIntegerInstruction (fromIntegral i) ctx)
instructionFromAST (ASTNodeSymbol s) ctx = putSymbolInstruction s ctx
instructionFromAST (ASTNodeSum x) ctx = putSumInstruction x ctx
instructionFromAST (ASTNodeSub x) ctx = putSubInstruction x ctx
instructionFromAST (ASTNodeMul x) ctx = putMulInstruction x ctx
instructionFromAST (ASTNodeDiv x) ctx = putDivInstruction x ctx
instructionFromAST (ASTNodeMod x) ctx = putModInstruction x ctx
instructionFromAST (ASTNodeDefine name children) ctx = putDefineInstruction name children ctx
instructionFromAST (ASTNodeError _) ctx = Nothing


putIntegerInstruction :: Int -> Context -> Context
putIntegerInstruction i ctx = ctx { instructions = (instructions ctx) ++ [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate i)]}

putSumInstruction :: [ASTNode] -> Context -> Maybe Context
putSumInstruction [x, y] ctx = do
    ctx' <- instructionFromAST x ctx
    ctx'' <- instructionFromAST y ctx' { instructions = (instructions ctx') ++ [Mov (Reg EDI) (Reg EAX)]}
    return (ctx'' { instructions = (instructions ctx'') ++ [Add (Reg EAX) (Reg EDI)]})

putSubInstruction :: [ASTNode] -> Context -> Maybe Context
putSubInstruction [x, y] ctx = do
    ctx' <- instructionFromAST x ctx
    ctx'' <- instructionFromAST y ctx' { instructions = (instructions ctx') ++ [Mov (Reg EDI) (Reg EAX)]}
    return (ctx'' { instructions = (instructions ctx'') ++ [Sub (Reg EAX) (Reg EDI)]})

putMulInstruction :: [ASTNode] -> Context -> Maybe Context
putMulInstruction [x, y] ctx = do
    ctx' <- instructionFromAST x ctx
    ctx'' <- instructionFromAST y ctx' { instructions = (instructions ctx') ++ [Mov (Reg EDI) (Reg EAX)]}
    return (ctx'' { instructions = (instructions ctx'') ++ [IMul (Reg EAX) (Reg EDI)]})

putDivInstruction :: [ASTNode] -> Context -> Maybe Context
putDivInstruction [x, y] ctx = do
    ctx' <- instructionFromAST x ctx
    ctx'' <- instructionFromAST y ctx' { instructions = (instructions ctx') ++ [Mov (Reg EDI) (Reg EAX)]}
    return (ctx'' { instructions = (instructions ctx'') ++ [Mov (Reg EBX) (Reg EAX), Mov (Reg EAX) (Reg EDI), Mov (Reg EDI) (Reg EBX), Div (Reg EDI)]})

putModInstruction :: [ASTNode] -> Context -> Maybe Context
putModInstruction [x, y] ctx = do
    ctx' <- instructionFromAST x ctx
    ctx'' <- instructionFromAST y ctx' { instructions = (instructions ctx') ++ [Mov (Reg EDI) (Reg EAX)]}
    return (ctx'' { instructions = (instructions ctx'') ++ [Mov (Reg EBX) (Reg EAX), Mov (Reg EAX) (Reg EDI), Mov (Reg EDI) (Reg EBX), Div (Reg EDI), Mov (Reg EAX) (Reg EDX)]})

putSymbolInstruction :: String -> Context -> Maybe Context
putSymbolInstruction s ctx = do
    let sym = symGet (Just ctx) s
    case sym of
        Just sym' -> return (ctx { instructions = (instructions ctx) ++ [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate sym')]})
        Nothing -> Nothing

putDefineInstruction :: ASTNode -> [ASTNode] -> Context -> Maybe Context
putDefineInstruction name children ctx = do
    let newCtx = instructionFromAST name ctx
    case newCtx of
        Just newCtx' -> Nothing
        Nothing -> do
            eax <-regGet (Just ctx) EAX
            let ctx' = stackPush (Just ctx) eax
            let pointer = fst (stackGetPointer ctx')
            symSet ctx' (astnsName name) pointer

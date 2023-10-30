module TestEvaluateAST
  ( testInstructionFromAST,
    testAstPush,
    testAstPushExec,
    testAstPushExec2,
    testAstPushExec3,
    testAstPushExec4,
    testAstPushExec5,
    testAstToInstr,
    testArrToHASMImpl,
    testArrToHASM,
    testStrToHASMImp,
    testStrToHASM,
    testMovStackAddrImpl,
    testMovStackAddr,
    testputDefineInstruction,
    testMovFromStackAddr,
    testFuncCall,
    testPutSymbolInstruction,
    testParamsRegister,
    testInvalidFuncs,
    testInferTypeFromNode,
    testPutSetNoErrCheck,
    testArraysFuncs
  )
where

import EvaluateAST
import Instructions
import Lexer
import Test.HUnit
import VM
import ValidState
import Tokenizer

testInstructionFromAST :: Test
testInstructionFromAST =
  TestList
    [ "instruction from ast Node interger" ~: instructionFromAST (ASTNodeInteger 123) (Valid newContext) ~?= Valid (newContext {instructions = [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 123)]}),
      "instruction from ast Node sum" ~: instructionFromAST (ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678]) (Valid newContext) ~?= Valid (newContext {instructions = [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 123), Push (Reg EAX), Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 678), Pop (Reg EDI), Add EAX (Reg EDI)]}),
      "instruction from ast Node sub" ~: instructionFromAST (ASTNodeSub [ASTNodeInteger 123, ASTNodeInteger 678]) (Valid newContext) ~?= Valid (newContext {instructions = [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 678), Push (Reg EAX), Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 123), Pop (Reg EDI), Sub (Reg EAX) (Reg EDI)]}),
      "instructions if statement" ~: instructionFromAST (ASTNodeIf (ASTNodeBoolean True) [ASTNodeInteger 1] (Invalid "nop")) (Valid newContext)
        ~?= Valid
          ( newContext
              { instructions =
                  [ Xor (Reg EAX) (Reg EAX),
                    Mov (Reg EAX) (Immediate 1),
                    Cmp (Reg EAX) (Immediate 1),
                    Cmp (Reg EAX) (Immediate 1),
                    Jne "_0else",
                    VM.Label "_0then" 6,
                    Xor (Reg EAX) (Reg EAX),
                    Mov (Reg EAX) (Immediate 1),
                    Jmp "_0end",
                    VM.Label "_0else" 9,
                    VM.Label "_0end" 11
                  ],
                uuids = 1
              }
          ),
      "instruction invalid context" ~: instructionFromAST (ASTNodeInteger 123) (Invalid "nop") ~?= Invalid "nop",
      "instruction AstSymbol" ~: instructionFromAST (ASTNodeSymbol "oui") (Valid newContext) ~?= Invalid "Symbol or Function not found: oui",
      -- "instruction if clause" ~: instructionFromAST (ASTNodeIf (ASTNodeArray []) thenBlock elseBlock) (Valid newContext)
      "instruction Eq" ~: instructionFromAST (ASTNodeEq [ASTNodeInteger 3, ASTNodeInteger 3]) (Valid newContext) ~?= putEqInstruction [ASTNodeInteger 3, ASTNodeInteger 3] (Valid newContext),
      "instruction Inferior" ~: instructionFromAST (ASTNodeInferior [ASTNodeInteger 4, ASTNodeInteger 4]) (Valid newContext) ~?= putInferiorInstruction [ASTNodeInteger 4, ASTNodeInteger 4] (Valid newContext),
      "instruction Superior" ~: instructionFromAST (ASTNodeSuperior [ASTNodeInteger 5, ASTNodeInteger 5]) (Valid newContext) ~?= putSuperiorInstruction [ASTNodeInteger 5, ASTNodeInteger 5] (Valid newContext),
      "instruction InferiorEq" ~: instructionFromAST (ASTNodeInferiorEq [ASTNodeInteger 6, ASTNodeInteger 6]) (Valid newContext) ~?= putInferiorEqInstruction [ASTNodeInteger 6, ASTNodeInteger 6] (Valid newContext),
      "instruction SuperiorEq" ~: instructionFromAST (ASTNodeSuperiorEq [ASTNodeInteger 7, ASTNodeInteger 7]) (Valid newContext) ~?= putSuperiorEqInstruction [ASTNodeInteger 7, ASTNodeInteger 7] (Valid newContext),
      "instruction NotEqual" ~: instructionFromAST (ASTNodeNotEqual [ASTNodeInteger 7, ASTNodeInteger 7]) (Valid newContext) ~?= putNotEqualInstruction [ASTNodeInteger 7, ASTNodeInteger 7] (Valid newContext),
      "instruction ParamList" ~: instructionFromAST (ASTNodeParamList []) (Valid newContext) ~?= Valid newContext,
      "instruction Mutable" ~: instructionFromAST (ASTNodeMutable (ASTNodeType GInt) (ASTNodeSymbol "var") (ASTNodeType GInt) (ASTNodeInteger 4)) (Valid newContext) ~?= putMutableInstruction GInt (ASTNodeSymbol "var") GInt (ASTNodeInteger 4) (Valid newContext),
      "instruction Node Array" ~: instructionFromAST (ASTNodeArray []) (Valid newContext) ~?= astNodeArrayToHASM (Valid newContext) (ASTNodeArray []),
      -- "instruction Print" ~: instructionFromAST (ASTNodePrint (ASTNodeInteger 4)) (Valid newContext {instructions = []}) ~?= putPrintInstruction (Valid newContext) (ASTNodePrint (ASTNodeInteger 4)),
      "instruction Bool" ~: instructionFromAST (ASTNodeBoolean False) (Valid newContext) ~?= putBoolInstruction 0 (Valid newContext),
      "instruction Define" ~: instructionFromAST (ASTNodeLambda (ASTNodeSymbol "func") (Valid (ASTNodeInteger 4)) []) (Valid newContext) ~?= putDefineInstruction (Valid newContext) (ASTNodeSymbol "func") (Valid (ASTNodeInteger 4)) [],
      "instruction While" ~: instructionFromAST (ASTNodeWhile (ASTNodeInteger 1) []) (Valid newContext) ~?= putWhileInstruction (Valid newContext) (ASTNodeInteger 1) [],
      "instruction Set" ~: instructionFromAST (ASTNodeSet (ASTNodeSymbol "test") (ASTNodeInteger 2)) (Valid newContext) ~?= putSetInstruction (Valid newContext) (ASTNodeSymbol "test") (ASTNodeInteger 2),
      "instruction Break" ~: instructionFromAST (ASTNodeBreak []) (Valid newContext) ~?= Valid newContext,
      "instruction Break" ~: instructionFromAST (ASTNodeBreak [ASTNodeInteger 4, ASTNodeInteger 4, ASTNodeInteger 4]) (Valid newContext) ~?= instructionFromAST (ASTNodeBreak [ASTNodeInteger 4, ASTNodeInteger 4]) (instructionFromAST (ASTNodeInteger 4) (Valid newContext)),
      "instruction Error" ~: instructionFromAST (ASTNodeError (TokenInfo TokenBool "String")) (Valid newContext) ~?= Invalid "Error: invalid AST(Error: String)"
    ]

testParamsRegister :: Test
testParamsRegister = TestList
  [
    "Should return 4 elem array" ~: paramsRegisters ~?= [EDI, ESI, EDX, ECX]
  ]

testAstPush :: Int
testAstPush =
  nbInstructions c
  where
    c = instructionFromAST (ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678]) (Valid newContext)

testAstPushExec :: Int
testAstPushExec =
  fromValidState (-1) (regGet context2 EAX)
  where
    context2 = execInstructions c
    c = instructionFromAST (ASTNodeSum [ASTNodeInteger 40, ASTNodeInteger 10]) (Valid newContext)

-- (+ 40 10)

testAstPushExec2 :: Int
testAstPushExec2 =
  fromValidState (-1) (regGet context2 EAX)
  where
    context2 = execInstructions c
    c = instructionFromAST (ASTNodeSub [(ASTNodeSum [ASTNodeInteger 40, ASTNodeInteger 10]), (ASTNodeSum [(ASTNodeSum [ASTNodeInteger 40, ASTNodeInteger 10]), (ASTNodeSum [ASTNodeInteger 40, ASTNodeInteger 10])])]) (Valid newContext)

-- (- (+ 40 10) (+ (+ 40 10) (+ 40 10))
testAstPushExec3 :: Int
testAstPushExec3 =
  fromValidState (-1) (regGet context2 EAX)
  where
    context2 = execInstructions c
    c = instructionFromAST (ASTNodeMul [ASTNodeInteger 5, ASTNodeInteger 10]) (Valid newContext)

-- (* 5 10)

testAstPushExec4 :: Int
testAstPushExec4 =
  fromValidState (-1) (regGet context2 EAX)
  where
    context2 = execInstructions c
    c = instructionFromAST (ASTNodeDiv [ASTNodeInteger 100, ASTNodeInteger 2]) (Valid newContext)

-- (/ 100 2)
testAstPushExec5 :: Int
testAstPushExec5 =
  fromValidState (-1) (regGet context2 EAX)
  where
    context2 = execInstructions c
    c = instructionFromAST (ASTNodeMod [ASTNodeInteger 10, ASTNodeInteger 4]) (Valid newContext)

-- (% 10 4)

testAstToInstr :: Test
testAstToInstr =
  TestList
    [ "Ast to Ctx push 3 instruction" ~: testAstPush ~?= 7,
      "Ast to Ctx push and execute multiple instructions" ~: testAstPushExec ~?= 50,
      "Ast to Ctx push and execute hardcore instructions" ~: testAstPushExec2 ~?= -50,
      "Ast to Ctx push and execute basic mult" ~: testAstPushExec3 ~?= 50,
      "Ast to Ctx push and execute basic div" ~: testAstPushExec4 ~?= 50,
      "Ast to Ctx push and execute basic mod" ~: testAstPushExec5 ~?= 2
    ]

testArrToHASMImpl :: [Instruction]
testArrToHASMImpl = case astNodeArrayToHASM (Valid newContext) (ASTNodeArray [ASTNodeInteger 1, ASTNodeInteger 2]) of
  Valid c -> instructions c
  Invalid _ -> []

testArrToHASM :: Test
testArrToHASM = TestList [
    "Array to ASM" ~: testArrToHASMImpl ~?= [Push (Reg EBX),Push (Reg ESI),Mov (Reg EAX) (Immediate 45),Alloc 2,Mov (Reg EBX) (Reg EAX),Mov (Reg ESI) (Reg EBX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 1),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 2),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 1),Mov (Reg EAX) (Reg EBX),Pop (Reg EBX),Pop (Reg ESI)]]

testStrToHASMImp :: String -> [Instruction]
testStrToHASMImp str = case strToHASM (Valid newContext) str of
  Valid c -> instructions c
  Invalid _ -> []

testStrToHASM :: Test
testStrToHASM = TestList [
    "(1) array" ~: testStrToHASMImp "(1)" ~?= [Enter,Push (Reg EBX),Push (Reg ESI),Mov (Reg EAX) (Immediate 45),Alloc 1,Mov (Reg EBX) (Reg EAX),Mov (Reg ESI) (Reg EBX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 1),Mov (Reg EAX) (Reg EBX),Pop (Reg EBX),Pop (Reg ESI)],
    "(1 2) array" ~: testStrToHASMImp "(1 2)" ~?= [Enter,Push (Reg EBX),Push (Reg ESI),Mov (Reg EAX) (Immediate 45),Alloc 2,Mov (Reg EBX) (Reg EAX),Mov (Reg ESI) (Reg EBX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 1),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 2),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 1),Mov (Reg EAX) (Reg EBX),Pop (Reg EBX),Pop (Reg ESI)],
    "(+ 1 2) sum" ~: testStrToHASMImp "(+ 1 2)" ~?= [Enter, Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),Push (Reg EAX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 2),Pop (Reg EDI),Add EAX (Reg EDI)]]
testMovStackAddrImpl :: [Int]
testMovStackAddrImpl = pile (stack c)
  where
    c = fromValidState newContext (movStackAddrImpl ctx (Immediate 2) (Immediate 1))
    ctx = stackPush (stackPush (stackPush (stackPush (Valid newContext) 0) 0) 0) 0

testMovStackAddr :: Test
testMovStackAddr =
  TestList
    [ "mov stack addr" ~: testMovStackAddrImpl ~?= [0, 0, 1, 0]
    ]

testputDefineInstruction :: Test
testputDefineInstruction =
  TestList
    [
      -- "instruction from ast Node define" ~: instructionFromAST (ASTNodeDefine (ASTNodeSymbol "oui") (Valid (ASTNodeInteger 42)) ([])) (Valid newContext) ~?= Valid newContext {instructions = [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 42), MovStackAddr (Immediate 0) (Reg EAX)], symbolTable = SymTable {symTable = [("oui", GInt)]}}
    ]

testMovFromStackAddr :: Test
testMovFromStackAddr =
  TestList
    [ "getting index two of the stack" ~: movFromStackAddrImpl (Valid newContext {stack = Stack [0, 1, 2, 3]}) (Reg EAX) (Immediate 2) ~?= regSet (Valid newContext {stack = Stack [0, 1, 2, 3]}) EAX 2
    ]

testFunCallNoArgs :: [Instruction]
testFunCallNoArgs = case strToHASM (Valid newContext) "(define foo 1) foo" of
  Valid c -> instructions c
  Invalid _ -> []

testFunCallArgs :: [Instruction]
testFunCallArgs = case strToHASM (Valid newContext) "(define foo (a) (1))(foo 1)" of
  Valid c -> instructions c
  Invalid _ -> []

testASTFunCallArgsImpl :: [ASTNode]
testASTFunCallArgsImpl = case strToHASM (Valid newContext) "(define foo (a) (1))(foo 1)" of
  Valid c -> cAST c
  Invalid _ -> [ASTNodeInteger 0]

testFuncCall :: Test
testFuncCall =
  TestList
    [ "ast fun call with arguments" ~: testASTFunCallArgsImpl
        ~?= [
              -- ASTNodeInstructionSequence [ASTNodeDefine (ASTNodeSymbol "foo") (Valid (ASTNodeParamList [ASTNodeSymbol "a"])) [ASTNodeInteger 1], ASTNodeArray ([ASTNodeSymbol "foo", ASTNodeInteger 1])]],
              ASTNodeInstructionSequence [ASTNodeDefine (ASTNodeSymbol "foo") (Valid (ASTNodeParamList [ASTNodeSymbol "a"])) [ASTNodeInteger 1], (ASTNodeFunctionCall "foo" [ASTNodeInteger 1])]
            ],
      "fun call no arguments: " ~: testFunCallNoArgs
        ~?= [ Enter,
              Call "foo"
            ],
      "fun call with arguments: " ~: testFunCallArgs
        ~?= [ Enter,
              Xor (Reg EAX) (Reg EAX),
              Mov (Reg EAX) (Immediate 1),
              Mov (Reg EDI) (Reg EAX),
              Call "foo"
            ]
    ]

testPutSymbolInstruction :: Test
testPutSymbolInstruction =
  TestList
    [ "instruction from ast Node symbol" ~: instructionFromAST (ASTNodeSymbol "oui") (Valid newContext) ~?= Invalid "Symbol or Function not found: oui"
    ]

testInvalidFuncs :: Test
testInvalidFuncs = TestList
  [
    "Invalid evalParamsToReg" ~: evalParamsToReg (Invalid "error") [] [] ~?= Invalid "error",
    "Invalid evalParamsToReg" ~: evalParamsToReg (Valid newContext) [ASTNodeInteger 3] [] ~?= Invalid "Error: too many parameters (max: 4)",
    "Invalid evalParamsToReg" ~: evalParamsToReg (Valid newContext) [ASTNodeError (TokenInfo TokenBool "String")] [EAX] ~?= Invalid "Error: invalid AST(Error: String)",
    "Invalid putFunctionCall" ~: putFunctionCall (Invalid "Error") "String" [] ~?= Invalid "Error",
    "Invalid putFunctionCall" ~: putFunctionCall (Valid newContext) "String" [ASTNodeError (TokenInfo TokenBool "String")] ~?= Invalid "Error: invalid AST(Error: String)",
    "Invalid pushParamTypeToBlock" ~: pushParamTypeToBlock (Invalid "Error") [] ~?= Invalid "Error",
    "Valid setupBlockParams" ~: setupBlockParams (Block "foo" (Valid newContext) []) (Valid (ASTNodeInteger 4)) ~?= Valid (Block "foo" (Valid newContext) []),
    "Invalid declSymbolBlock" ~: declSymbolBlock (Block "foo" (Valid newContext) []) [ASTNodeInteger 4] ~?= Invalid "Error: invalid parameter: expected symbol",
    "Invalid evaluateBlock" ~: evaluateBlock (Invalid "Error") (Block "foo" (Valid newContext) []) (Valid (ASTNodeInteger 4)) [] ~?= Invalid "Error",
    "Invalid evaluateBlock" ~: evaluateBlock (Valid newContext) (Block "foo" (Valid newContext) []) (Valid (ASTNodeInteger 4)) [ASTNodeError (TokenInfo TokenBool "String")] ~?= Invalid "Error: invalid AST(Error: String)",
    "Invalid evaluateBlockOneInstr" ~: evaluateBlockOneInstr (Invalid "Error") (Block "foo" (Valid newContext) []) (Invalid "Error") (ASTNodeInteger 4) ~?= Invalid "Error",
    "Invalid copyParentBlocks" ~: copyParentBlocks newContext (Block "foo" (Invalid "Error") []) ~?= Block "foo" (Invalid "Error") [],
    "Invalid putDefineInstruction" ~: putDefineInstruction (Invalid "Error") (ASTNodeInteger 3) (Invalid "Error") [] ~?= Invalid "While defining function: \n\tError",
    "Invalid putDefineInstruction" ~: putDefineInstruction (Valid newContext) (ASTNodeSymbol "Funct") (Valid (ASTNodeBoolean True)) [ASTNodeArray [ASTNodeInteger 4]] ~?= putDefineInstruction (Valid newContext) (ASTNodeSymbol "Funct") (Valid (ASTNodeBoolean True)) [ASTNodeInteger 4],
    "Invalid putPrintInstruction" ~: putPrintInstruction (Invalid "Error") (ASTNodeInteger 4) ~?= Invalid "Error",
    "Valid putPrintInstruction" ~: putPrintInstruction (Valid newContext) (ASTNodeInteger 4) ~?= Valid newContext {instructions = [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 4), Xor (Reg EBX) (Reg EBX), Mov (Reg EBX) (Immediate 2), Xor (Reg ECX) (Reg ECX), Mov (Reg ECX) (Reg EAX), Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 4), Interrupt]},
    "Invalid putInstructionSequence" ~: putInstructionSequence [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putIntegerInstruction" ~: putIntegerInstruction 3 (Invalid "Error") ~?= Invalid "Error",
    "Invalid putBoolInstruction" ~: putBoolInstruction 3 (Invalid "Error") ~?= Invalid "Error",
    "Invalid putSumInstruction" ~: putSumInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putSumInstruction Error" ~: putSumInstruction [] (Valid newContext) ~?= Invalid "Error",
    "Invalid putEqInstruction" ~: putEqInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putEqInstruction Error" ~: putEqInstruction [] (Valid newContext) ~?= Invalid "Error",
    "Invalid putNotEqualInstruction" ~: putNotEqualInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putInferiorInstruction" ~: putInferiorInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putInferiorInstruction Error" ~: putInferiorInstruction [] (Valid newContext) ~?= Invalid "Error",
    "Invalid putInferiorEqInstruction" ~: putInferiorEqInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putInferiorEqInstruction Error" ~: putInferiorEqInstruction [] (Valid newContext) ~?= Invalid "Error",
    "Invalid putSuperiorEqInstruction" ~: putSuperiorEqInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putSuperiorEqInstruction Error" ~: putSuperiorEqInstruction [] (Valid newContext) ~?= Invalid "Error",
    "Invalid putSuperiorInstruction" ~: putSuperiorInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putSuperiorInstruction Error" ~: putSuperiorInstruction [] (Valid newContext) ~?= Invalid "Error",
    "Invalid putSubInstruction" ~: putSubInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putSubInstruction Error" ~: putSubInstruction [] (Valid newContext) ~?= Invalid "Error",
    "Invalid putMulInstruction" ~: putMulInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putMulInstruction Error" ~: putMulInstruction [] (Valid newContext) ~?= Invalid "Error",
    "Invalid putDivInstruction" ~: putDivInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putDivInstruction Error" ~: putDivInstruction [] (Valid newContext) ~?= Invalid "Error",
    "Invalid putModInstruction" ~: putModInstruction [] (Invalid "Error") ~?= Invalid "Error",
    "Invalid putModInstruction Error" ~: putModInstruction [] (Valid newContext) ~?= Invalid "Error",
    "Invalid tryPutFunctionCall" ~: tryPutFunctionCall (Invalid "Error") "Function" ~?= Invalid "Error",
    "Invalid putSymbolInstruction" ~: putSymbolInstruction "jaja" (Invalid "Error") ~?= Invalid "Error",
    "Invalid putWhileInstruction" ~: putWhileInstruction (Invalid "Error") (ASTNodeInteger 4) [] ~?= Invalid "Error",
    "Invalid putWhileCondition" ~: putWhileCondition (Invalid "Error") (ASTNodeInteger 4) 4 ~?= Invalid "Error",
    "Invalid putMutableNoErrCheck" ~: putMutableNoErrCheck GInt (ASTNodeInteger 2) (ASTNodeInteger 2) (Invalid "Error") ~?= Invalid "Error",
    "Invalid putMutableNoErrCheck" ~: putMutableNoErrCheck GInt (ASTNodeSymbol "test") (ASTNodeError (TokenInfo TokenBool "String")) (Valid newContext) ~?= Invalid "Error: invalid AST(Error: String)",
    "Invalid putMutableInstruction" ~: putMutableInstruction GInt (ASTNodeInteger 2) GInt (ASTNodeInteger 2) (Invalid "Error") ~?= Invalid "Error",
    "Invalid putSetInstruction" ~: putSetInstruction (Invalid "Error") (ASTNodeInteger 2) (ASTNodeInteger 2) ~?= Invalid "Error",
    "Valid putSetInstruction" ~: putSetInstruction (Valid newContext) (ASTNodeSymbol "Test") (ASTNodeInteger 2) ~?= Invalid "Error: Variable does't exists",
    -- "Valid putSetInstruction" ~: putSetInstruction (Valid newContext) (ASTNodeSymbol "Test2432") (ASTNodeInteger 2) ~?= putSetNoErrCheck (Valid newContext) (ASTNodeSymbol "Test2432") (ASTNodeInteger 2),
    "Invalid putIfInstruction" ~: putIfInstruction (Invalid "Error") (ASTNodeInteger 1) ~?= Invalid "Error",
    "Invalid putIfInstruction" ~: putIfInstruction (Valid newContext) (ASTNodeInteger 1) ~?= Invalid "Invalid arguments to if clause",
    "Invalid ifPutCondition" ~: ifPutCondition (Invalid "Error") (ASTNodeBoolean True) 0 ~?= Invalid "Error",
    "Invalid ifPutCondition" ~: ifPutCondition (Valid newContext) (ASTNodeError (TokenInfo TokenBool "String")) 0 ~?= Invalid "Error: invalid AST(Error: String)",
    "Invalid astNodeArrayToHASM" ~: astNodeArrayToHASM (Invalid "Error") (ASTNodeBoolean True) ~?= Invalid "Error",
    "Invalid astNodeArrayToHASM" ~: astNodeArrayToHASM (Valid newContext) (ASTNodeBoolean True) ~?= Invalid "Error: could not resolve array",
    "Invalid aSTNodeArrayToHASMPreLoop" ~: aSTNodeArrayToHASMPreLoop (Invalid "Error") [] ~?= Invalid "Error",
    "Invalid astNodeArrayToHASMEnd" ~: astNodeArrayToHASMEnd (Invalid "Error") ~?= Invalid "Error",
    "Invalid strToHASM" ~: strToHASM (Invalid "Error") "test" ~?= Invalid "Error",
    "Invalid astNodeArrayToHASMLoopBody" ~: astNodeArrayToHASMLoopBody (Invalid "Error") [] ~?= Invalid "Error",
    "Valid astNodeArrayToHASMLoopBody" ~: astNodeArrayToHASMLoopBody (Valid newContext) [] ~?= Valid newContext
  ]

testInferTypeFromNode :: Test
testInferTypeFromNode = TestList
  [
    "Invalid is undefined" ~: inferTypeFromNode (Invalid "Error") (ASTNodeInteger 4) ~?= GUndefinedType,
    "Valid is int" ~: inferTypeFromNode (Valid newContext) (ASTNodeInteger 4) ~?= GInt,
    "Valid is bool" ~: inferTypeFromNode (Valid newContext) (ASTNodeBoolean True) ~?= GBool,
    "Valid symbol not found" ~: inferTypeFromNode (Valid newContext) (ASTNodeSymbol "jee") ~?= GUndefinedType,
    "Sum is int" ~: inferTypeFromNode (Valid newContext) (ASTNodeSum [ASTNodeInteger 3, ASTNodeInteger 4]) ~?= GInt,
    "Sub is int" ~: inferTypeFromNode (Valid newContext) (ASTNodeSub [ASTNodeInteger 3, ASTNodeInteger 4]) ~?= GInt,
    "Mul is int" ~: inferTypeFromNode (Valid newContext) (ASTNodeMul [ASTNodeInteger 3, ASTNodeInteger 4]) ~?= GInt,
    "Div is int" ~: inferTypeFromNode (Valid newContext) (ASTNodeDiv [ASTNodeInteger 3, ASTNodeInteger 4]) ~?= GInt,
    "Mod is int" ~: inferTypeFromNode (Valid newContext) (ASTNodeMod [ASTNodeInteger 3, ASTNodeInteger 4]) ~?= GInt,
    "Array is a ptr" ~: inferTypeFromNode (Valid newContext) (ASTNodeArray []) ~?= GPtr,
    "Undefined has no type" ~: inferTypeFromNode (Valid newContext) (ASTNodeIf (ASTNodeBoolean True) [] (Invalid "")) ~?= GUndefinedType
  ]

testPutSetNoErrCheck :: Test
testPutSetNoErrCheck = TestList
  [
    "Invalid putSetNoErrCheck" ~: putSetNoErrCheck (Invalid "Error") (ASTNodeBoolean True) (ASTNodeBoolean False) ~?= Invalid "Error"
  ]

testArraysFuncs :: Test
testArraysFuncs = TestList
 [
    "hasmBackupRegisters" ~: hasmBackupRegisters [] ~?= [],
    "hasmBackupRegisters full" ~: hasmBackupRegisters [EAX] ~?= [Push (Reg EAX)],
    "hasmRestoreRegisters" ~: hasmRestoreRegisters [] ~?= [],
    "hasmRestoreRegisters full" ~: hasmRestoreRegisters [EAX] ~?= [Pop (Reg EAX)],
    "labelImpl" ~: labelImpl (Valid newContext) "test" 5 ~?= labelSet (Valid newContext) "test" 5
 ]

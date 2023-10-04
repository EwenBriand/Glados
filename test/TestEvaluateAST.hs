module TestEvaluateAST (
    testInstructionFromAST,
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
    testPutSymbolInstruction
) where

import Test.HUnit
import EvaluateAST
import VM
import Instructions
import Lexer
import ValidState


testInstructionFromAST :: Test
testInstructionFromAST =
  TestList
    [ "instruction from ast Node interger" ~: instructionFromAST (ASTNodeInteger 123) (Valid newContext) ~?= Valid (newContext {instructions = [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 123)]}),
      "instruction from ast Node sum" ~: instructionFromAST (ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678]) (Valid newContext) ~?= Valid (newContext {instructions = [Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 123),Push (Reg EAX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 678),Pop (Reg EDI),Add EAX (Reg EDI)]}),
      "instruction from ast Node sub" ~: instructionFromAST (ASTNodeSub [ASTNodeInteger 123, ASTNodeInteger 678]) (Valid newContext) ~?= Valid (newContext {instructions = [Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 678),Push (Reg EAX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 123),Pop (Reg EDI),Sub (Reg EAX) (Reg EDI)]}),
      "instructions if statement" ~: instructionFromAST (ASTNodeIf (ASTNodeBoolean True) [ASTNodeInteger 1] (Invalid "nop")) (Valid newContext) ~?= Valid (newContext {instructions = [
        Xor (Reg EAX) (Reg EAX),
        Mov (Reg EAX) (Immediate 1),
        Cmp (Reg EAX) (Immediate 0),
        Je "0else",
        VM.Label "0then" 5,
        Xor (Reg EAX) (Reg EAX),
        Mov (Reg EAX) (Immediate 1),
        Jmp "0end",
        VM.Label "0else" 8,
        VM.Label "0end" 10
        ], uuids = 1}),
      "instruction invalid context" ~: instructionFromAST (ASTNodeInteger 123) (Invalid "nop") ~?= Invalid "nop",
      "instruction AstSymbol" ~: instructionFromAST (ASTNodeSymbol "oui") (Valid newContext) ~?= Valid (newContext {instructions = [Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 0),MovStackAddr (Immediate 0) (Reg EAX)]})
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
    Invalid s -> []

testArrToHASM :: Test
testArrToHASM = TestList [
    "Array to ASM" ~: testArrToHASMImpl ~?= [Push (Reg EBX),Push (Reg ESI),Mov (Reg EAX) (Immediate 45),Mov (Reg EBX) (Immediate 8),Interrupt,Mov (Reg EBX) (Reg EAX),Mov (Reg ESI) (Reg EBX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 4),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 2),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 4),Mov (Reg EAX) (Reg EBX),Pop (Reg EBX),Pop (Reg ESI)]]

testStrToHASMImp :: String -> [Instruction]
testStrToHASMImp str = case strToHASM (Valid newContext) str of
    Valid c -> instructions c
    Invalid s -> []

testStrToHASM :: Test
testStrToHASM = TestList [
    "(1) array" ~: testStrToHASMImp "(1)" ~?= [Enter, Push (Reg EBX),Push (Reg ESI),Mov (Reg EAX) (Immediate 45),Mov (Reg EBX) (Immediate 4),Interrupt,Mov (Reg EBX) (Reg EAX),Mov (Reg ESI) (Reg EBX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 4),Mov (Reg EAX) (Reg EBX),Pop (Reg EBX),Pop (Reg ESI)],
    "(1 2) array" ~: testStrToHASMImp "(1 2)" ~?= [Enter, Push (Reg EBX),Push (Reg ESI),Mov (Reg EAX) (Immediate 45),Mov (Reg EBX) (Immediate 8),Interrupt,Mov (Reg EBX) (Reg EAX),Mov (Reg ESI) (Reg EBX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 4),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 2),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 4),Mov (Reg EAX) (Reg EBX),Pop (Reg EBX),Pop (Reg ESI)],
    "(+ 1 2) sum" ~: testStrToHASMImp "(+ 1 2)" ~?= [Enter, Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),Push (Reg EAX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 2),Pop (Reg EDI),Add EAX (Reg EDI)]]

testMovStackAddrImpl :: [Int]
testMovStackAddrImpl = pile (stack c)
    where
        c = fromValidState newContext (movStackAddrImpl ctx (Immediate 2) (Immediate 1))
        ctx = stackPush (stackPush (stackPush (stackPush (Valid newContext) 0) 0) 0) 0

testMovStackAddr :: Test
testMovStackAddr = TestList [
    "mov stack addr" ~: testMovStackAddrImpl ~?= [0, 0, 1, 0]]

testputDefineInstruction :: Test
testputDefineInstruction = TestList [
      "instruction from ast Node define" ~: instructionFromAST (ASTNodeDefine (ASTNodeSymbol "oui") (ASTNodeInteger 42)) (Valid newContext) ~?= Valid newContext {instructions = [Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 42),MovStackAddr (Immediate 0) (Reg EAX)], symbolTable = SymTable {symTable = [("oui", GInt)]}}]

testMovFromStackAddr :: Test
testMovFromStackAddr = TestList [
      "getting index two of the stack" ~: movFromStackAddrImpl (Valid newContext {stack = Stack [0, 1, 2, 3]}) (Reg EAX) (Immediate 2) ~?= regSet (Valid newContext {stack = Stack [0, 1, 2, 3]}) EAX 2]

testPutSymbolInstruction :: Test
testPutSymbolInstruction = TestList [
      "instruction from ast Node symbol" ~: instructionFromAST (ASTNodeSymbol "oui") (Valid newContext) ~?= Valid newContext {instructions = [Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 0),MovStackAddr (Immediate 0) (Reg EAX)]}
      ]

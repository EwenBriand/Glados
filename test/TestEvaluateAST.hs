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
    testMovFromStackAddr
) where

import Test.HUnit
import EvaluateAST
import VM
import Lexer
import Instructions
import qualified Data.Maybe as Data


testInstructionFromAST :: Test
testInstructionFromAST =
  TestList
    [ "instruction from ast Node interger" ~: instructionFromAST (ASTNodeInteger 123) (Just newContext) ~?= Just (newContext {instructions = [Xor (Reg EAX) (Reg EAX), Mov (Reg EAX) (Immediate 123)]}),
      "instruction from ast Node sum" ~: instructionFromAST (ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678]) (Just newContext) ~?= Just (newContext {instructions = [Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 123),Push (Reg EAX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 678),Pop (Reg EDI),Add EAX (Reg EDI)]}),
      "instruction from ast Node sub" ~: instructionFromAST (ASTNodeSub [ASTNodeInteger 123, ASTNodeInteger 678]) (Just newContext) ~?= Just (newContext {instructions = [Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 678),Push (Reg EAX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 123),Pop (Reg EDI),Sub (Reg EAX) (Reg EDI)]})
    ]

testAstPush :: Int
testAstPush =
  nbInstructions c
  where
    c = instructionFromAST (ASTNodeSum [ASTNodeInteger 123, ASTNodeInteger 678]) (Just newContext)

testAstPushExec :: Int
testAstPushExec =
  Data.fromMaybe (-1) (regGet context2 EAX)
  where
    context2 = execInstructions c
    c = instructionFromAST (ASTNodeSum [ASTNodeInteger 40, ASTNodeInteger 10]) (Just newContext)

-- (+ 40 10)

testAstPushExec2 :: Int
testAstPushExec2 =
  Data.fromMaybe (-1) (regGet context2 EAX)
  where
    context2 = execInstructions c
    c = instructionFromAST (ASTNodeSub [(ASTNodeSum [ASTNodeInteger 40, ASTNodeInteger 10]), (ASTNodeSum [(ASTNodeSum [ASTNodeInteger 40, ASTNodeInteger 10]), (ASTNodeSum [ASTNodeInteger 40, ASTNodeInteger 10])])]) (Just newContext)

-- (- (+ 40 10) (+ (+ 40 10) (+ 40 10))
testAstPushExec3 :: Int
testAstPushExec3 =
  Data.fromMaybe (-1) (regGet context2 EAX)
  where
    context2 = execInstructions c
    c = instructionFromAST (ASTNodeMul [ASTNodeInteger 5, ASTNodeInteger 10]) (Just newContext)

-- (* 5 10)

testAstPushExec4 :: Int
testAstPushExec4 =
  Data.fromMaybe (-1) (regGet context2 EAX)
  where
    context2 = execInstructions c
    c = instructionFromAST (ASTNodeDiv [ASTNodeInteger 100, ASTNodeInteger 2]) (Just newContext)

-- (/ 100 2)
testAstPushExec5 :: Int
testAstPushExec5 =
  Data.fromMaybe (-1) (regGet context2 EAX)
  where
    context2 = execInstructions c
    c = instructionFromAST (ASTNodeMod [ASTNodeInteger 10, ASTNodeInteger 4]) (Just newContext)

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
testArrToHASMImpl = maybe [] instructions (astNodeArrayToHASM
    (Just newContext)
    (ASTNodeArray [ASTNodeInteger 1, ASTNodeInteger 2]))

testArrToHASM :: Test
testArrToHASM = TestList [
    "Array to ASM" ~: testArrToHASMImpl ~?= [Push (Reg EBX),Push (Reg ESI),Mov (Reg EAX) (Immediate 45),Mov (Reg EBX) (Immediate 8),Interrupt,Mov (Reg EBX) (Reg EAX),Mov (Reg ESI) (Reg EBX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 4),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 2),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 4),Mov (Reg EAX) (Reg EBX),Pop (Reg EBX),Pop (Reg ESI)]]

testStrToHASMImp :: String -> [Instruction]
testStrToHASMImp str = maybe [] instructions (strToHASM (Just newContext) str)

testStrToHASM :: Test
testStrToHASM = TestList [
    "(1 2) array" ~: testStrToHASMImp "(1 2)" ~?= [Enter, Push (Reg EBX),Push (Reg ESI),Mov (Reg EAX) (Immediate 45),Mov (Reg EBX) (Immediate 8),Interrupt,Mov (Reg EBX) (Reg EAX),Mov (Reg ESI) (Reg EBX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 4),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 2),MovPtr (Reg ESI) (Reg EAX),Add ESI (Immediate 4),Mov (Reg EAX) (Reg EBX),Pop (Reg EBX),Pop (Reg ESI)],
    "(+ 1 2) sum" ~: testStrToHASMImp "(+ 1 2)" ~?= [Enter, Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 1),Push (Reg EAX),Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 2),Pop (Reg EDI),Add EAX (Reg EDI)]]

testMovStackAddrImpl :: [Int]
testMovStackAddrImpl = pile (stack c)
    where
        c = Data.fromMaybe newContext (movStackAddrImpl ctx (Immediate 2) (Immediate 1))
        ctx = stackPush (stackPush (stackPush (stackPush (Just newContext) 0) 0) 0) 0

testMovStackAddr :: Test
testMovStackAddr = TestList [
    "mov stack addr" ~: testMovStackAddrImpl ~?= [0, 0, 1, 0]]

testputDefineInstruction :: Test
testputDefineInstruction = TestList [
      "instruction from ast Node define" ~: instructionFromAST (ASTNodeDefine (ASTNodeSymbol "oui") (ASTNodeInteger 42)) (Just newContext) ~?= Just newContext {instructions = [Xor (Reg EAX) (Reg EAX),Mov (Reg EAX) (Immediate 42),MovStackAddr (Immediate 0) (Reg EAX)], symbolTable = SymTable {symTable = [("oui",4)]}}]

testMovFromStackAddr :: Test
testMovFromStackAddr = TestList [
      "getting index two of the stack" ~: movFromStackAddrImpl (Just newContext {stack = Stack [0, 1, 2, 3]}) (Reg EAX) (Immediate 2) ~?= regSet (Just newContext {stack = Stack [0, 1, 2, 3]}) EAX 2]

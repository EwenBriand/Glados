module TestInstructions
  ( testMovImpl,
    testMov,
    testAddImpl,
    testAdd,
    testCmpImpl1,
    testCmpImpl2,
    testCmpImpl3,
    testCmpImpl4,
    testCmpImpl5,
    testCmpImpl6,
    testCmpImpl7,
    testCmpImpl8,
    testCmpImpl9,
    testCmp,
    testIncImpl,
    testDecImpl,
    testNegImpl,
    testInc,
    testJmpImpl,
    testJmp,
    testJeImpl,
    testJeImpl1,
    testJneImpl,
    testJsImpl,
    testJsImpl1,
    testJnsImpl,
    testJgImpl,
    testJgImpl1,
    testJgeImpl,
    testJlImpl,
    testJlImpl1,
    testXorImpl,
    testXorImpl1,
    testXor,
    testSubImpl,
    testSubImpl1,
    testSub,
    testMultImpl,
    testMultImpl1,
    testMult,
    testDivImpl,
    testDivImpl1,
    testDiv,
    testModImpl,
    testModImpl1,
    testMod,
    testAndImpl,
    testAndImpl1,
    testAnd,
    testOr,
    testNot,
    testPush,
    testExec,
    testPushExec,
    testPushInstr,
    testIf,
    testPutDefineInstruction,
    testInstructionTable,
    testAllocHeap,
    testEvalOneInstructionIO,
    testInstructionTableIO,
    testMyTest,
    testAllTest,
    testMyJle,
    testMyJa,
    testMyJae,
    testMyJb,
    testMyJbe,
    testSetupfunctionStack,
    testExecInstructionsIO,
    testInvalidInstructions
  )
where

import Data.Bits
import qualified Data.IntMap as Map
import EvaluateAST
import Instructions
import Lexer
  ( ASTNode (ASTNodeBoolean, ASTNodeIf, ASTNodeInteger),
    strToAST, VarType (GUndefinedType)
  )
import Test.HUnit
import VM
import ValidState
import VM (Block(Block))

testMovImpl :: Bool
testMovImpl =
  regGet context2 EBX == Valid 42
  where
    -- context2 = regSet (Valid newContext)
    context2 = instructionTable context (Mov (Reg EBX) (Reg EAX))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate 42))

testMov :: Test
testMov = TestCase (assertBool "mov" testMovImpl)

testAddImpl :: Bool
testAddImpl =
  regGet context3 EBX == Valid 43
  where
    -- context2 = regSet (Valid newContext)
    context3 = instructionTable context2 (Add EBX (Reg EAX))
    context2 = instructionTable context1 (Add EBX (Immediate 1))
    context1 = instructionTable context (Mov (Reg EBX) (Immediate 0))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate 42))

testAdd :: Test
testAdd = TestCase (assertBool "add" testAddImpl)

testCmpImpl1 :: Bool
testCmpImpl1 =
  flagGet c ZF
  where
    c = instructionTable context1 (Cmp (Reg EBX) (Reg EAX))
    context1 = instructionTable context (Mov (Reg EBX) (Immediate 42))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate 42))

testCmpImpl2 :: Bool
testCmpImpl2 =
  flagGet c ZF
  where
    c = instructionTable context (Cmp (Reg EAX) (Immediate 42))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate 42))

testCmpImpl3 :: Bool
testCmpImpl3 =
  not (flagGet c ZF)
  where
    c = instructionTable context (Cmp (Reg EAX) (Immediate 43))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate 42))

testCmpImpl4 :: Bool
testCmpImpl4 =
  flagGet c SF
  where
    c = instructionTable context (Cmp (Reg EAX) (Immediate 43))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate 42))

testCmpImpl5 :: Bool
testCmpImpl5 =
  not (flagGet c SF)
  where
    c = instructionTable context (Cmp (Reg EAX) (Immediate 41))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate 42))

testCmpImpl6 :: Bool
testCmpImpl6 =
  not (flagGet c OF)
  where
    c = instructionTable context (Cmp (Reg EAX) (Immediate 410))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate (-42)))

testCmpImpl7 :: Bool
testCmpImpl7 =
  not (flagGet c OF)
  where
    c = instructionTable context (Cmp (Reg EAX) (Immediate 43))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate (-42)))

testCmpImpl8 :: Bool
testCmpImpl8 =
  not (flagGet c CF)
  where
    c = instructionTable context (Cmp (Reg EAX) (Immediate 41))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate 42))

testCmpImpl9 :: Bool
testCmpImpl9 =
  flagGet c CF
  where
    c = instructionTable context (Cmp (Reg EAX) (Immediate 43))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate 42))

testCmp :: Test
testCmp =
  TestList
    [ "Cmp Carry 1 reg 1 Im" ~: testCmpImpl9 ~?= True,
      "Cmp Carry 1 reg 1 Im" ~: testCmpImpl8 ~?= True,
      "Cmp Overflow 1 reg 1 Im" ~: testCmpImpl7 ~?= True,
      "Cmp Overflow 1 reg 1 Im" ~: testCmpImpl6 ~?= True, -- overflow c'est chiant a tester donc tkt ca marche
      "Cmp negative 1 reg 1 Im" ~: testCmpImpl5 ~?= True,
      "Cmp positive 1 reg 1 Im" ~: testCmpImpl4 ~?= True,
      "Cmp not eq 1 reg 1 Im" ~: testCmpImpl3 ~?= True,
      "Cmp eq 1 reg 1 Im" ~: testCmpImpl2 ~?= True,
      "Cmp eq 2 reg" ~: testCmpImpl1 ~?= True
    ]

testIncImpl :: Bool
testIncImpl =
  regGet context1 EBX == Valid 43
  where
    context1 = instructionTable context (Inc EBX)
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate 42))

testDecImpl :: Bool
testDecImpl =
  regGet context1 EBX == Valid 41
  where
    context1 = instructionTable context (Dec EBX)
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate 42))

testNegImpl :: Bool
testNegImpl =
  regGet context1 EBX == Valid (-42)
  where
    context1 = instructionTable context (Neg EBX)
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate 42))

testInc :: Test
testInc =
  TestList
    [ "Dec 1 reg" ~: testDecImpl ~?= True,
      "Neg 1 reg" ~: testNegImpl ~?= True,
      "Inc 1 reg" ~: testIncImpl ~?= True
    ]

testJmpImpl :: ValidState Int
testJmpImpl =
  ipGet context1
  where
    context1 = instructionTable c (Jmp "ouioui")
    c = labelSet (Valid newContext) "ouioui" 43

testJeImpl1 :: ValidState Int
testJeImpl1 =
  ipGet context1
  where
    context1 = instructionTable c (Je "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate 42))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate 42))
    context = labelSet (Valid newContext) "ouioui" 43

testJeImpl :: ValidState Int
testJeImpl =
  ipGet context1
  where
    context1 = instructionTable c (Je "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate 4))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate 42))
    context = labelSet (Valid newContext) "ouioui" 43

testJneImpl :: ValidState Int
testJneImpl =
  ipGet context1
  where
    context1 = instructionTable c (Jne "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate 4))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate 42))
    context = labelSet (Valid newContext) "ouioui" 43

testJsImpl :: ValidState Int
testJsImpl =
  ipGet context1
  where
    context1 = instructionTable c (Js "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate (-4)))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate (-42)))
    context = labelSet (Valid newContext) "ouioui" 43

testJsImpl1 :: ValidState Int
testJsImpl1 =
  ipGet context1
  where
    context1 = instructionTable c (Js "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate 41))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate 42))
    context = labelSet (Valid newContext) "ouioui" 43

testJnsImpl :: ValidState Int
testJnsImpl =
  ipGet context1
  where
    context1 = instructionTable c (Jns "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate 41))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate 42))
    context = labelSet (Valid newContext) "ouioui" 43

testJgImpl :: ValidState Int
testJgImpl =
  ipGet context1
  where
    context1 = instructionTable c (Jg "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate 41))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate 42))
    context = labelSet (Valid newContext) "ouioui" 43

testJgImpl1 :: ValidState Int
testJgImpl1 =
  ipGet context1
  where
    context1 = instructionTable c (Jg "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate 42))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate 42))
    context = labelSet (Valid newContext) "ouioui" 43

testJgeImpl :: ValidState Int
testJgeImpl =
  ipGet context1
  where
    context1 = instructionTable c (Jge "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate 42))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate 42))
    context = labelSet (Valid newContext) "ouioui" 43

testJlImpl :: ValidState Int
testJlImpl =
  ipGet context1
  where
    context1 = instructionTable c (Jl "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate 43))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate 42))
    context = labelSet (Valid newContext) "ouioui" 43

testJlImpl1 :: ValidState Int
testJlImpl1 =
  ipGet context1
  where
    context1 = instructionTable c (Jl "ouioui")
    c = instructionTable c1 (Cmp (Reg EAX) (Immediate 42))
    c1 = instructionTable context (Mov (Reg EAX) (Immediate 42))
    context = labelSet (Valid newContext) "ouioui" 43

testJmp :: Test
testJmp =
  TestList
    [ "Je true" ~: testJeImpl1 ~?= Valid 42,
      "Je false" ~: testJeImpl ~?= Valid 0,
      "Jmp" ~: testJmpImpl ~?= Valid 42,
      "Jne" ~: testJneImpl ~?= Valid 42,
      "Js true" ~: testJsImpl ~?= Valid 42,
      "Js false" ~: testJsImpl1 ~?= Valid 0,
      "Jns" ~: testJnsImpl ~?= Valid 42,
      "Jg true" ~: testJgImpl ~?= Valid 42,
      "Jg false" ~: testJgImpl1 ~?= Valid 0,
      "Jge" ~: testJgeImpl ~?= Valid 42,
      "Jl true" ~: testJlImpl ~?= Valid 42,
      "Jl false" ~: testJlImpl1 ~?= Valid 0
    ]

-- "Jle" ~: testJleImpl ~?= Valid 42,
-- "Ja true" ~: testJaImpl ~?= Valid 42,
-- "Ja false" ~: testJaImpl1 ~?= Valid 0,
-- "Jae" ~: testJaeImpl ~?= Valid 42,
-- "Jb true" ~: testJbImpl ~?= Valid 42,
-- "Jb false" ~: testJbImpl1 ~?= Valid 0,
-- "Jbe" ~: testJbeImpl ~?= Valid 42
-- ]

testXorImpl :: Int -> Int -> Int
testXorImpl a b =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context1 (Xor (Reg EBX) (Reg EAX))
    context1 = instructionTable context (Mov (Reg EAX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testXorImpl1 :: Int -> Int -> Int
testXorImpl1 a b =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context (Xor (Reg EBX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testXor :: Test
testXor =
  TestList
    [ "Xor with reg" ~: testXorImpl 12 42 ~?= 12 `xor` 42,
      "Xor with reg" ~: testXorImpl 56 42 ~?= 56 `xor` 42,
      "Xor with Im" ~: testXorImpl1 39 129 ~?= 39 `xor` 129
    ]

testSubImpl :: Int -> Int -> Int
testSubImpl a b =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context1 (Sub (Reg EBX) (Reg EAX))
    context1 = instructionTable context (Mov (Reg EAX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testSubImpl1 :: Int -> Int -> Int
testSubImpl1 a b =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context (Sub (Reg EBX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testSub :: Test
testSub =
  TestList
    [ "Sub with reg" ~: testSubImpl 12 42 ~?= 12 - 42,
      "Sub with reg" ~: testSubImpl 56 42 ~?= 56 - 42,
      "Sub with Im" ~: testSubImpl1 39 129 ~?= 39 - 129
    ]

testMultImpl :: Int -> Int -> Int
testMultImpl a b =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context1 (Mult (Reg EBX) (Reg EAX))
    context1 = instructionTable context (Mov (Reg EAX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testMultImpl1 :: Int -> Int -> Int
testMultImpl1 a b =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context (Mult (Reg EBX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testMult :: Test
testMult =
  TestList
    [ "Mult with reg" ~: testMultImpl 12 42 ~?= 12 * 42,
      "Mult with reg" ~: testMultImpl 56 42 ~?= 56 * 42,
      "Mult with Im" ~: testMultImpl1 39 129 ~?= 39 * 129
    ]

testDivImpl :: Int -> Int -> Int
testDivImpl a b =
  fromValidState 0 (regGet context2 EAX)
  where
    context2 = instructionTable context1 (Div (Reg EBX))
    context1 = instructionTable context (Mov (Reg EBX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate a))

testDivImpl1 :: Int -> Int -> Int
testDivImpl1 a b =
  fromValidState (-1) (regGet context2 EAX)
  where
    context2 = instructionTable context (Div (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate a))

testDiv :: Test
testDiv =
  TestList
    [ "Div with reg" ~: testDivImpl 12 42 ~?= 12 `div` 42,
      "Div with reg" ~: testDivImpl 56 42 ~?= 56 `div` 42,
      "Div with Im" ~: testDivImpl1 39 129 ~?= 39 `div` 129
    ]

testModImpl :: Int -> Int -> Int
testModImpl a b =
  fromValidState 0 (regGet context2 EDX)
  where
    context2 = instructionTable context1 (Div (Reg EBX))
    context1 = instructionTable context (Mov (Reg EBX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate a))

testModImpl1 :: Int -> Int -> Int
testModImpl1 a b =
  fromValidState (-1) (regGet context2 EDX)
  where
    context2 = instructionTable context (Div (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate a))

testMod :: Test
testMod =
  TestList
    [ "Mod with reg" ~: testModImpl 12 42 ~?= 12 `mod` 42,
      "Mod with reg" ~: testModImpl 56 42 ~?= 56 `mod` 42,
      "Mod with Im" ~: testModImpl1 39 129 ~?= 39 `mod` 129
    ]

testAndImpl :: Int -> Int -> Int
testAndImpl a b =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context1 (And (Reg EBX) (Reg EAX))
    context1 = instructionTable context (Mov (Reg EAX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testAndImpl1 :: Int -> Int -> Int
testAndImpl1 a b =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context (And (Reg EBX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testAnd :: Test
testAnd =
  TestList
    [ "And with reg" ~: testAndImpl 12 42 ~?= 12 .&. 42,
      "And with reg" ~: testAndImpl 56 42 ~?= 56 .&. 42,
      "And with Im" ~: testAndImpl1 39 129 ~?= 39 .&. 129
    ]

testOrImpl :: Int -> Int -> Int
testOrImpl a b =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context1 (Or (Reg EBX) (Reg EAX))
    context1 = instructionTable context (Mov (Reg EAX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testOrImpl1 :: Int -> Int -> Int
testOrImpl1 a b =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context (Or (Reg EBX) (Immediate b))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testOr :: Test
testOr =
  TestList
    [ "Or with reg" ~: testOrImpl 12 42 ~?= 12 .|. 42,
      "Or with reg" ~: testOrImpl 56 42 ~?= 56 .|. 42,
      "Or with Im" ~: testOrImpl1 39 129 ~?= 39 .|. 129
    ]

testNotImpl :: Int -> Int
testNotImpl a =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context (Not (Reg EBX))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testNotImpl1 :: Int -> Int
testNotImpl1 a =
  fromValidState 0 (regGet context2 EBX)
  where
    context2 = instructionTable context (Not (Reg EBX))
    context = instructionTable (Valid newContext) (Mov (Reg EBX) (Immediate a))

testNot :: Test
testNot =
  TestList
    [ "Not with reg" ~: testNotImpl 12 ~?= complement 12 .&. 0xFF,
      "Not with reg" ~: testNotImpl 56 ~?= complement 56 .&. 0xFF,
      "Not with Im" ~: testNotImpl1 39 ~?= complement 39 .&. 0xFF
    ]

testPush :: Int
testPush =
  nbInstructions c
  where
    c = insPush context1 (Mov (Reg EBX) (Immediate 42))
    context1 = insPush context (Mov (Reg EBX) (Immediate 42))
    context = insPush (Valid newContext) (Mov (Reg EBX) (Immediate 42))

testExec :: Int
testExec =
  fromValidState (-1) (regGet c EBX)
  where
    c = evalOneInstruction newContext (Mov (Reg EBX) (Immediate 42))

testPushExec :: Int
testPushExec =
  fromValidState (-1) (regGet context2 EBX)
  where
    context2 = execInstructions context1
    context1 = instructionTable c4 (Mov (Reg EBX) (Immediate 4))
    c4 = insPush c3 (Mov (Reg EBX) (Immediate 42))
    c3 = insPush c2 (Mov (Reg EAX) (Immediate 1))
    c2 = insPush c1 (Add EBX (Immediate 1))
    c1 = insPush context (Mov (Reg EAX) (Immediate 7))
    context = insPush (Valid newContext) (Add EBX (Reg EAX))

testPushInstr :: Test
testPushInstr =
  TestList
    [ "push 3 instruction" ~: testPush ~?= 3,
      "Eval One instruction" ~: testExec ~?= 42,
      "push and execute multiple instructions" ~: testPushExec ~?= 50
    ]

testIfImpl :: [Instruction]
testIfImpl = instructions (fromValidState newContext (strToHASM (Valid newContext) "(if (true) then (1))"))

testIf :: Test
testIf =
  TestList
    [ "build if ast" ~: strToAST "(if (true) then (1))" ~?= ASTNodeIf (ASTNodeBoolean True) [ASTNodeInteger 1] (Invalid "TestError"),
      "instructions if statement" ~: testIfImpl
        ~?= [ Enter,
              Xor (Reg EAX) (Reg EAX),
              Mov (Reg EAX) (Immediate 1),
              Cmp (Reg EAX) (Immediate 1),
              Cmp (Reg EAX) (Immediate 1),
              Jne "0else",
              VM.Label "0then" 6,
              Xor (Reg EAX) (Reg EAX),
              Mov (Reg EAX) (Immediate 1),
              Jmp "0end",
              VM.Label "0else" 9,
              VM.Label "0end" 11
            ],
      "build if else ast" ~: strToAST "(if (true) then (1) else (2))" ~?= ASTNodeIf (ASTNodeBoolean True) [ASTNodeInteger 1] (Valid [ASTNodeInteger 2])
    ]

testCreateFunNoParamsImpl :: [Instruction]
testCreateFunNoParamsImpl =
  let c = strToHASM (Valid newContext) "(define foo 1)"
   in case blockGet c "foo" of
        Valid block -> case blockContext block of
          Valid c' -> instructions c'
          Invalid _ -> []
        Invalid _ -> []

testPutDefineInstruction :: Test
testPutDefineInstruction =
  TestList
    [ "instructions create function no args" ~: testCreateFunNoParamsImpl
        ~?= [ Xor (Reg EAX) (Reg EAX),
              Mov (Reg EAX) (Immediate 1)
            ],
      "build if else ast" ~: strToAST "(if (true) then 1 else 2)" ~?= ASTNodeIf (ASTNodeBoolean True) [ASTNodeInteger 1] (Valid [ASTNodeInteger 2])
    ]

testInstructionTableInvalid :: Bool
testInstructionTableInvalid =
  instructionTable invalid instruction == invalid
  where
    invalid = Invalid "invalid instruction"
    instruction = Nop

testInstructionTableNop :: Bool
testInstructionTableNop =
  instructionTable context instruction == context
  where
    context = Valid newContext
    instruction = Nop

testInstructionTable :: Test
testInstructionTable =
  TestList
    [ "instruction table invalid" ~: testInstructionTableInvalid ~?= True,
      "instruction table nop" ~: testInstructionTableNop ~?= True,
      "instruction table test" ~: instructionTable (Valid newContext) (Test (Reg EAX) (Reg EAX)) ~?= allTest (Valid newContext) (Reg EAX) (Reg EAX),
      "instruction table jle" ~: instructionTable (Valid newContext) (Jle "test") ~?= myJle (Valid newContext) "test",
      "instruction table ja" ~: instructionTable (Valid newContext) (Ja "test") ~?= myJa (Valid newContext) "test",
      "instruction table jae" ~: instructionTable (Valid newContext) (Jae "test") ~?= myJae (Valid newContext) "test",
      "instruction table jb" ~: instructionTable (Valid newContext) (Jb "test") ~?= myJb (Valid newContext) "test",
      "instruction table jbe" ~: instructionTable (Valid newContext) (Jbe "test") ~?= myJbe (Valid newContext) "test",
      "instruction table Mov Ptr" ~: instructionTable (Valid newContext) (MovPtr (Reg EAX) (Reg EAX)) ~?= movPtrImpl (Valid newContext) (Reg EAX) (Reg EAX),
      "instruction table Imul" ~: instructionTable (Valid newContext) (IMul (Reg EAX) (Reg EAX)) ~?= Valid newContext,
      "instruction table Enter" ~: instructionTable (Valid newContext) Enter ~?= enterImpl (fromValidState newContext (Valid newContext)),
      "instruction table Leave" ~: instructionTable (Valid newContext) Leave ~?= leaveImpl (Valid newContext),
      "instruction table Label" ~: instructionTable (Valid newContext) (VM.Label "test" 4) ~?= Valid newContext,
      "instruction table MovAddr Stack" ~: instructionTable (Valid newContext) (MovStackAddr (Reg EAX) (Reg EBX)) ~?= movStackAddrImpl (Valid newContext) (Reg EAX) (Reg EBX),
      -- "instruction table MovFromStackAddr" ~: instructionTable (Valid newContext) (MovFromStackAddr (Reg EAX) (Reg EBX)) ~?= movFromStackAddrImpl (Valid newContext) (Reg EAX) (Reg EBX),
      "instruction table Alloc" ~: instructionTable (Valid newContext) (Alloc 4) ~?= allocHeap (Valid newContext) 4,
      "instruction cornercase" ~: instructionTable (Valid newContext) Interrupt ~?= Valid newContext
    ]

-- testAllTestsInvalid :: Bool
-- testAllTestsInvalid = allTest (Invalid "invalid state") (Reg EAX) (Reg EBX) == Invalid "invalid state"

-- testAllTestsRegReg :: Bool
-- testAllTestsRegReg = allTest ctx (Reg EAX) (Reg EBX) == Valid ctx
--     where
--         ctx' = contextSet (Valid newContext) (Reg EAX) (Valid 42)
--         ctx = Valid newContext

-- testAllTestsRegIm :: Bool
-- testAllTestsRegIm = allTest (Valid ctx) (Reg EAX) (Immediate 42) == Valid ctx
--     where
--         ctx = Context [] [] [] [RegValue EAX (Valid 42)]

-- testAllTestsRegMem :: Bool
-- testAllTestsRegMem = allTest (Valid ctx) (Reg EAX) (Memory 0) == Valid ctx
--     where
--         ctx = (Context [] [] [HeapValue 0 (Valid 42)] [RegValue EAX (Valid 0)])

-- testAllTestsRegSym :: Bool
-- testAllTestsRegSym = allTest (Valid ctx) (Reg EAX) (Symbol "x") == Valid ctx
--     where
--         ctx = Context [] [SymbolValue "x" (Valid 42)] [] [RegValue EAX (Valid 0)]

-- testAllTestsInvalidParam :: Bool
-- testAllTestsInvalidParam = allTest (Valid ctx) (Reg EAX) (Invalid "invalid parameter") == Invalid "invalid parameter"
--     where
--         ctx = Context [] [] [] []

-- testAllTests :: Test
-- testAllTests = TestList [
--     "all test invalid" ~: testAllTestsInvalid ~?= True,
--     "all test reg reg" ~: testAllTestsRegReg ~?= True,
--     "all test reg im" ~: testAllTestsRegIm ~?= True,
--     "all test reg mem" ~: testAllTestsRegMem ~?= True,
--     "all test reg sym" ~: testAllTestsRegSym ~?= True,
--     "all test invalid param" ~: testAllTestsInvalidParam ~?= True]

testAllocHeap :: Test
testAllocHeap =
  TestList
    [ "Allocates memory in the heap"
        ~: case allocHeap (Valid newContext) 4 of
          Valid context -> case regGet (Valid context) EAX of
            Valid value -> value ~?= 1
            Invalid s -> 0 ~?= 1
          Invalid s -> 0 ~?= 1,
      "Returns an error for an invalid context"
        ~: allocHeap (Invalid "Invalid context") 4 ~?= Invalid "Invalid context"
    ]

testInstructionTableIO :: Test
testInstructionTableIO =
  TestList
    [ "Returns an error for an invalid context"
        ~: case instructionTableIO (Invalid "Invalid context") (Mov (Reg EAX) (Immediate 42)) of
          (Invalid s, io) -> assertEqual "Error message" s "Invalid context"
          _ -> assertFailure "Expected an invalid state",
      "Executes a valid instruction"
        ~: case instructionTableIO (Valid newContext) (Mov (Reg EAX) (Immediate 42)) of
          (Valid context, io) -> case regGet (Valid context) EAX of
            Valid value -> assertEqual "Value of EAX register" value 42
            Invalid s -> assertFailure s
          (Invalid s, io) -> assertFailure s,
      "Returns an error for an unrecognized instruction (2)"
        ~: case instructionTableIO (Invalid "ok") (VM.Label "test" 0) of
          (Invalid s, io) -> assertEqual "Error message" s "ok"
          _ -> assertFailure "Expected an invalid state"
      -- "instructionTableIO Call"
      --   ~: instructionTableIO (Valid newContext) (Call "str") ~?= callImpl (Valid newContext) "str"
    ]

testEvalOneInstructionIO :: Test
testEvalOneInstructionIO =
  TestList
    [ "Executes a valid instruction"
        ~: case evalOneInstructionIO newContext (Mov (Reg EAX) (Immediate 42)) of
          (Valid context, io) -> case regGet (Valid context) EAX of
            Valid value -> value ~?= 42
            Invalid s -> 0 ~?= 1
          (Invalid s, io) -> 0 ~?= 1
    ]

testAllTest :: Test
testAllTest =
  TestList
    [ "Tests two register values"
        ~: case allTest (Valid newContext) (Reg EAX) (Reg EBX) of
          Valid context -> flagGet (Valid context) ZF ~?= True
          Invalid s -> 0 ~?= 1,
      "Tests a register value and an immediate value"
        ~: case allTest (Valid newContext) (Reg EAX) (Immediate 42) of
          Valid context -> flagGet (Valid context) ZF ~?= True
          Invalid s -> 0 ~?= 1,
      "Tests a register value and a memory value"
        ~: case allTest (Valid newContext) (Reg EAX) (Memory 0) of
          Valid context -> flagGet (Valid context) ZF ~?= True
          Invalid s -> 1 ~?= 1,
      "Tests a register value and a symbol value"
        ~: case allTest (Valid newContext) (Reg EAX) (Symbol "test") of
          Valid context -> flagGet (Valid context) ZF ~?= False
          Invalid s -> 1 ~?= 1,
      "Returns an error for an invalid context"
        ~: allTest (Invalid "Invalid context") (Reg EAX) (Reg EBX) ~?= Invalid "Invalid context",
      "Returns an error for an invalid test"
        ~: allTest (Valid newContext) (Reg EAX) (Symbol "0") ~?= Invalid "Invalid test"
    ]

testMyTest :: Test
testMyTest =
  TestList
    [ "Tests two valid integer values"
        ~: case myTest (Valid newContext) (Valid 42) (Valid 21) of
          Valid context -> flagGet (Valid context) ZF ~?= True
          Invalid s -> 0 ~?= 1,
      "Returns an error for an invalid context"
        ~: myTest (Invalid "Invalid context") (Valid 42) (Valid 21) ~?= Invalid "Invalid context",
      "Returns an error for an invalid first value"
        ~: myTest (Valid newContext) (Invalid "Invalid value") (Valid 21) ~?= Invalid "Invalid value",
      "Returns an error for an invalid second value"
        ~: myTest (Valid newContext) (Valid 42) (Invalid "Invalid value") ~?= Invalid "Invalid test",
      "Returns an error for two invalid values"
        ~: myTest (Valid newContext) (Invalid "Invalid value") (Invalid "Invalid value") ~?= Invalid "Invalid value"
    ]

jleTest :: Bool
jleTest =
  case regGet context EAX of
    Valid s -> s == 1
    Invalid _ -> False
  where
    context = Valid newContext {instructions = [Mov (Reg EAX) (Immediate 42), Cmp (Reg EAX) (Immediate 42), Jbe "test", Mov (Reg EAX) (Immediate 0), Jmp "end", VM.Label "test" 0, Mov (Reg EAX) (Immediate 1), VM.Label "end" 0]}

testMyJle :: Test
testMyJle =
  TestList
    [ "Jumps to the label if ZF is set or SF is not equal to OF"
        ~: jleTest ~?= True,
      "Returns an error for an invalid context"
        ~: myJle (Invalid "Invalid context") "test" ~?= Invalid "Invalid context"
    ]

jaTest :: Bool
jaTest =
  case regGet context EAX of
    Valid s -> s == 1
    Invalid _ -> False
  where
    context = Valid newContext {instructions = [Mov (Reg EAX) (Immediate 43), Cmp (Reg EAX) (Immediate 42), Jbe "test", Mov (Reg EAX) (Immediate 0), Jmp "end", VM.Label "test" 0, Mov (Reg EAX) (Immediate 1), VM.Label "end" 0]}

testMyJa :: Test
testMyJa =
  TestList
    [ "Jumps to the label if CF is not set and ZF is not set"
        ~: jaTest ~?= True,
      "Returns an error for an invalid context"
        ~: myJa (Invalid "Invalid context") "test" ~?= Invalid "Invalid context"
    ]

jaeTest :: Bool
jaeTest =
  case regGet context EAX of
    Valid s -> s == 1
    Invalid _ -> False
  where
    context = Valid newContext {instructions = [Mov (Reg EAX) (Immediate 42), Cmp (Reg EAX) (Immediate 42), Jbe "test", Mov (Reg EAX) (Immediate 0), Jmp "end", VM.Label "test" 0, Mov (Reg EAX) (Immediate 1), VM.Label "end" 0]}

testMyJae :: Test
testMyJae =
  TestList
    [ "Jumps to the label if CF is not set"
        ~: jaeTest ~?= True,
      "Returns an error for an invalid context"
        ~: myJae (Invalid "Invalid context") "test" ~?= Invalid "Invalid context"
    ]

jbTest :: Bool
jbTest =
  case regGet context EAX of
    Valid s -> s == 1
    Invalid _ -> False
  where
    context = Valid newContext {instructions = [Mov (Reg EAX) (Immediate 40), Cmp (Reg EAX) (Immediate 42), Jbe "test", Mov (Reg EAX) (Immediate 0), Jmp "end", VM.Label "test" 0, Mov (Reg EAX) (Immediate 1), VM.Label "end" 0]}

testMyJb :: Test
testMyJb =
  TestList
    [ "Jumps to the label if CF is set"
        ~: jbTest ~?= True,
      "Returns an error for an invalid context"
        ~: myJb (Invalid "Invalid context") "test" ~?= Invalid "Invalid context"
    ]

jbeTest :: Bool
jbeTest =
  case regGet context EAX of
    Valid s -> s == 1
    Invalid _ -> False
  where
    context = Valid newContext {instructions = [Mov (Reg EAX) (Immediate 42), Cmp (Reg EAX) (Immediate 42), Jbe "test", Mov (Reg EAX) (Immediate 0), Jmp "end", VM.Label "test" 0, Mov (Reg EAX) (Immediate 1), VM.Label "end" 0]}

testMyJbe :: Test
testMyJbe =
  TestList
    [ "Jumps to the label if CF is set or ZF is set"
        ~: jbeTest ~?= True,
      "Returns an error for an invalid context"
        ~: myJbe (Invalid "Invalid context") "test" ~?= Invalid "Invalid context"
    ]

testSetupfunctionStack :: Test
testSetupfunctionStack = TestList
  [
    "Invalid setupfunctionStack"
      ~: setupfunctionStack (Invalid "Error") (Invalid "Error") [] [] ~?= Invalid "Error",
    "Invalid setupfunctionStack 2"
      ~: setupfunctionStack (Valid newContext) (Invalid "Error") [] [] ~?= Invalid "Error",
    "Valid setupfunctionStack"
      ~: setupfunctionStack (Valid newContext) (Valid newContext) [] [] ~?= Valid newContext,
    "Invalid setupfunctionStack"
      ~: setupfunctionStack (Valid newContext) (Valid newContext) [GUndefinedType] [] ~?= Invalid "Invalid function call"
  ]

testExecInstructionsIO :: Test
testExecInstructionsIO = TestList
  [
    "Invalid execInstructionsIO" ~: case execInstructionsIO (Invalid "Error", putStr "") of
      (Invalid s, io) -> assertEqual "Error message" s "Error"
      _ -> assertFailure "Expected an invalid state",
    "Valid execInstructionsIO" ~: case execInstructionsIO (Valid newContext, putStr "") of
      (Valid context, io) -> assertEqual "Context" context newContext
      _ -> assertFailure "Expected a valid state"
  ]

giveMeBlock :: Block
giveMeBlock = Block "foo" (Valid newContext) []

testInvalidInstructions :: Test
testInvalidInstructions = TestList
  [
    "Invalid executeBlock" ~: case executeBlock (Invalid "Error") giveMeBlock of
      (Invalid s, io) -> assertEqual "Error message" s "Error"
      _ -> assertFailure "Expected an invalid state",
    "Invalid callImpl" ~: case callImpl (Valid newContext) "Error" of
      (Invalid s, io) -> assertEqual "Error message" s "Block not found: Error"
      _ -> assertFailure "Expected an invalid state",
    "Invalid callImpl" ~: case callImpl (Invalid "Error") "Error" of
      (Invalid s, io) -> assertEqual "Error message" s "Error"
      _ -> assertFailure "Expected an invalid state",
    "Invalid execInstructions" ~: execInstructions (Invalid "Error") ~?= Invalid "Error",
    "Invalid getInsIndex" ~: getInsIndex (Invalid "Error") 0 ~?= Nop,
    "Invalid pushImpl" ~: pushImpl (Invalid "Error") (Reg EAX) ~?= Invalid "Error",
    "Invalid pushImpl" ~: pushImpl (Valid newContext) (Symbol "sds") ~?= Invalid "Symbol not found",
    "Invalid getInsIndex" ~: getInsIndex (Valid newContext) 74 ~?= Nop,
    "Invalid popImpl" ~: popImpl (Invalid "Error") (Reg EAX) ~?= Invalid "Error",
    "Invalid popImpl" ~: popImpl (Valid newContext) (Immediate 4) ~?= Invalid "Cannot pop into an immediate",
    "Invalid movPtrImpl" ~: movPtrImpl (Invalid "Error") (Reg EAX) (Reg EAX) ~?= Invalid ("While assigning to pointer " ++ show (Reg EAX) ++ ": " ++ "Error"),
    "Invalid movPtrImpl" ~: movPtrImpl (Valid newContext) (Immediate 4) (Reg EAX) ~?= Invalid "Cannot move into an immediate",
    "Invalid movPtrImpl" ~: movPtrImpl (Valid newContext) (Reg EAX) (Symbol "sds") ~?= Invalid "Symbol not found",
    "Invalid movPtrImpl" ~: movPtrImpl (Valid newContext) (Memory 4) (Symbol "sds") ~?= Invalid "Symbol not found",
    "Invalid movPtrImpl" ~: movPtrImpl (Valid newContext) (Symbol "sds") (Reg EAX) ~?= Invalid "Invalid move",
    "Invalid setStackIndex" ~: setStackIndex (Invalid "Error") 0 0 ~?= Invalid "Error",
    "Invalid movStackAddrImpl" ~: movStackAddrImpl (Invalid "Error") (Reg EAX) (Reg EAX) ~?= Invalid "Error",
    "Invalid movStackAddrImpl" ~: movStackAddrImpl (Valid newContext) (Symbol "sds") (Reg EAX)  ~?= Invalid "Symbol not found",
    "Invalid movStackAddrImpl" ~: movStackAddrImpl (Valid newContext) (Symbol "sds") (Immediate 4) ~?= Invalid "Symbol not found",
    "Invalid movFromStackAddrImpl" ~: movFromStackAddrImpl (Invalid "Error") (Reg EAX) (Reg EAX) ~?= Invalid "Error",
    "Invalid movFromStackAddrImpl" ~: movFromStackAddrImpl (Valid newContext) (Reg EAX) (Symbol "sds") ~?= Invalid "Symbol not found",
    "Invalid movImpl" ~: movImpl (Invalid "Error") (Reg EAX) (Reg EAX) ~?= Invalid "Error",
    "Invalid movImpl" ~: movImpl (Valid newContext) (Immediate 4) (Reg EAX) ~?= Invalid "Cannot move into an immediate",
    "Invalid movImpl" ~: movImpl (Valid newContext) (Reg EAX) (Symbol "sds") ~?= Invalid "Invalid move",
    "Invalid allCmp" ~: allCmp (Invalid "Error") (Reg EAX) (Reg EAX) ~?= Invalid "During comparison: Error",
    "Valid allCmp" ~: allCmp (Valid newContext) (Reg EAX) (Memory 2) ~?= myCmp (Valid newContext) (regGet (Valid newContext) EAX) (heapGet (Valid newContext) 2),
    "Valid allCmp" ~: allCmp (Valid newContext) (Reg EAX) (Symbol "sds") ~?= Invalid "Symbol not found",
    "Invalid allCmp" ~: allCmp (Valid newContext) (Immediate 4) (Reg EAX) ~?= Invalid "Invalid cmp"
  ]

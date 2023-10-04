module TestInstructions (
    testMovImpl,
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
    testPutDefineInstruction
) where

import Test.HUnit
import Instructions
import VM
import ValidState
import Data.Bits
import EvaluateAST
import Lexer


testMovImpl :: Bool
testMovImpl =
    regGet context2 EBX == Valid 42
    where
        -- context2 = regSet (Valid newContext)
        context2 = instructionTable context ( (Mov (Reg EBX) (Reg EAX)))
        context = instructionTable (Valid newContext) ( (Mov (Reg EAX) (Immediate 42)))

testMov :: Test
testMov = TestCase (assertBool "mov" testMovImpl)

testAddImpl :: Bool
testAddImpl =
    regGet context3 EBX == Valid 43
    where
        -- context2 = regSet (Valid newContext)
        context3 = instructionTable context2 ( (Add EBX (Reg EAX)))
        context2 = instructionTable context1 ( (Add EBX (Immediate 1)))
        context1 = instructionTable context ( (Mov (Reg EBX) (Immediate 0)))
        context = instructionTable (Valid newContext) ( (Mov (Reg EAX) (Immediate 42)))

testAdd :: Test
testAdd = TestCase (assertBool "add" testAddImpl)

testCmpImpl1 :: Bool
testCmpImpl1 =
    if (flagGet c ZF == True) then True else False
    where
        c = instructionTable context1 ( (Cmp (Reg EBX) (Reg EAX)))
        context1 = instructionTable context ( (Mov (Reg EBX) (Immediate 42)))
        context = instructionTable (Valid newContext) ( (Mov (Reg EAX) (Immediate 42)))

testCmpImpl2 :: Bool
testCmpImpl2 =
    if (flagGet c ZF == True) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 42)))
        context = instructionTable (Valid newContext) ( (Mov (Reg EAX) (Immediate 42)))

testCmpImpl3 :: Bool
testCmpImpl3 =
    if (flagGet c ZF == False) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 43)))
        context = instructionTable (Valid newContext) ( (Mov (Reg EAX) (Immediate 42)))

testCmpImpl4 :: Bool
testCmpImpl4 =
    if (flagGet c SF == True) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 43)))
        context = instructionTable (Valid newContext) ( (Mov (Reg EAX) (Immediate 42)))

testCmpImpl5 :: Bool
testCmpImpl5 =
    if (flagGet c SF == False) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 41)))
        context = instructionTable (Valid newContext) ( (Mov (Reg EAX) (Immediate 42)))

testCmpImpl6 :: Bool
testCmpImpl6 =
    if (flagGet c OF == False) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 410)))
        context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate (-42)))

testCmpImpl7 :: Bool
testCmpImpl7 =
    if (flagGet c OF == False) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 43)))
        context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate (-42)))

testCmpImpl8 :: Bool
testCmpImpl8 =
    if (flagGet c CF == False) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 41)))
        context = instructionTable (Valid newContext) (Mov (Reg EAX) (Immediate 42))

testCmpImpl9 :: Bool
testCmpImpl9 =
    if (flagGet c CF == True) then True else False
    where
        c = instructionTable context ( (Cmp (Reg EAX) (Immediate 43)))
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
        context = instructionTable (Valid newContext) ( (Mov (Reg EBX) (Immediate 42)))

testDecImpl :: Bool
testDecImpl =
    regGet context1 EBX == Valid 41
    where
        context1 = instructionTable context (Dec EBX)
        context = instructionTable (Valid newContext) ( (Mov (Reg EBX) (Immediate 42)))

testNegImpl :: Bool
testNegImpl =
    regGet context1 EBX == Valid (-42)
    where
        context1 = instructionTable context (Neg EBX)
        context = instructionTable (Valid newContext) ( (Mov (Reg EBX) (Immediate 42)))

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
      "Jl false" ~: testJlImpl1 ~?= Valid 0]
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
testIf = TestList [
    "build if ast" ~: strToAST "(if (true) then (1))" ~?= ASTNodeIf (ASTNodeBoolean True) [ASTNodeInteger 1] (Invalid "TestError"),
    "instructions if statement" ~: testIfImpl ~?= [
        Enter,
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
        ],
        "build if else ast" ~: strToAST "(if (true) then (1) else (2))" ~?= ASTNodeIf (ASTNodeBoolean True) [ASTNodeInteger 1] (Valid [ASTNodeInteger 2])]

testCreateFunNoParamsImpl :: [Instruction]
testCreateFunNoParamsImpl = let c = strToHASM (Valid newContext) "(define foo 1)" in
    case blockGet c "foo" of
        Valid block -> case blockContext block of
            Valid c' -> instructions c'
            Invalid _ -> []
        Invalid _ -> []

testPutDefineInstruction :: Test
testPutDefineInstruction = TestList [
    "instructions create function no args" ~: testCreateFunNoParamsImpl ~?= [
        Xor (Reg EAX) (Reg EAX),
        Mov (Reg EAX) (Immediate 1)]
    ]

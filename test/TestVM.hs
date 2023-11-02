module TestVM
  ( testIncRegister,
    testLoadContext,
    testIncRegisterInvalidContext,
    testDecRegister,
    testDecRegisterInvalidContext,
    testAddRegister,
    testAddRegisterInvalidContext,
    testSubRegister,
    testSubRegisterInvalidContext,
    testMulRegister,
    testMulRegisterInvalidContext,
    testDivRegister,
    testDivRegisterInvalidContext,
    testDivZeroRegister,
    testModRegister,
    testModRegisterInvalidContext,
    testModZeroRegister,
    testAndRegister,
    testAndRegisterInvalidContext,
    testOrRegister,
    testBlockInitAllocVarSpace,
    testOrRegisterInvalidContext,
    testXorRegister,
    testParseLabels,
    testSetTruValueFromParam2,
    testXorRegisterInvalidContext,
    testNotRegister,
    testNotRegisterInvalidContext,
    testStackPushPop,
    testStackPushPopPeek,
    testStackDup,
    testStackSwap,
    testStackRot,
    testHeapAlloc,
    testHeapAllocBis,
    testHeapSetGet,
    testLabelSetGet,
    testFlagGetSet,
    testCodeFromValidStateInt,
    testExecSyscall,
    testExecSyscallWrapper,
    testCodeFromEAX,
    testCallEasyPrint,
    testBlock,
    testBlockMap,
    testBlockShow,
    testBlockAddInvalid,
    testSymTable,
    testHeap,
    testStackGetPointer,
    testStackPeek,
    testStackDup2,
    testStackSwap2,
    testStackRot2,
    testStackPush,
    testStackPop,
    testParam,
    testInstruction,
    testGetTrueValueFromParam,
    testStackClear,
    testRegInvalids,
    testSetTrueValueFromParam,
    testEq,
    testShow,
    testOrd,
    testStackGetValueFromIndex,
    testAddressDoesntExist,
    testInvalidHeap,
    testSymGet,
    testAdaptValueToVarType,
    testSysPrintValue,
    testsLabelsFuncs,
    testFlagInvalid,
    testContext,
    testNextUUIDValid,
    testIps,
    testHasmNStackPush,
    testLabels,
    functionalVMTests
  )
where

import qualified Data.Map as Map
import GHC.Generics (Fixity (Prefix), FixityI (PrefixI))
import Lexer (VarType (GBool, GInt, GVoid))
import Test.HUnit
import VM
import ValidState
import System.IO.Silently
import VM (Instruction(Test))

testRegInvalids :: Test
testRegInvalids = TestList
  [
    "RegSetInvalid" ~:  regSet (Invalid "Error") EAX 1 ~?= Invalid "Error",
    "RegGetInvalid" ~:  regGet (Invalid "Error") EAX ~?= Invalid "Error",
    "RegGetNothing" ~:  regGet (Valid newContext {registers = Registers (Map.fromList [(EAX, 42)])}) EDX ~?= Invalid "Register not found",
    "RegNotInvalid" ~:  regNot (Invalid "Error") EAX ~?= Invalid "Error",
    "RegIncInvalid" ~:  regInc (Invalid "Error") EAX ~?= Invalid "Error",
    "RegDecInvalid" ~:  regDec (Invalid "Error") EAX ~?= Invalid "Error",
    "RegAddInvalid" ~:  regAdd (Invalid "Error") EAX 1 ~?= Invalid "Error",
    "RegSubInvalid" ~:  regSub (Invalid "Error") EAX 1 ~?= Invalid "Error",
    "RegMulInvalid" ~:  regMul (Invalid "Error") EAX 1 ~?= Invalid "Error",
    "RegDivInvalid" ~:  regDiv (Invalid "Error") EAX 1 ~?= Invalid "Error",
    "RegDivInvalidDiv" ~:  regDiv (regSet (Valid newContext) EAX 9) EAX 0 ~?= Invalid "Division by zero",
    "RegModInvalid" ~:  regMod (Invalid "Error") EAX 1 ~?= Invalid "Error",
    "RegModInvalidMod" ~:  regMod (regSet (Valid newContext) EAX 9) EAX 0 ~?= Invalid "Modulo by zero",
    "RegAndInvalid" ~: regAnd (Invalid "Error") EAX 1 ~?= Invalid "Error",
    "RegOrInvalid" ~: regOr (Invalid "Error") EAX 1 ~?= Invalid "Error",
    "RegXorInvalid" ~: regXor (Invalid "Error") EAX 1 ~?= Invalid "Error"
  ]

testIncRegisterImpl :: Bool
testIncRegisterImpl =
  regGet context EAX == Valid 1
  where
    context = regInc (Valid newContext) EAX

testIncRegister :: Test
testIncRegister = TestCase (assertBool "inc register" testIncRegisterImpl)

testIncRegisterInvalidContextImpl :: Bool
testIncRegisterInvalidContextImpl =
  case regInc (Invalid "err") EAX of
    (Invalid _) -> True
    _ -> False

testIncRegisterInvalidContext :: Test
testIncRegisterInvalidContext = TestCase (assertBool "inc register invalid context" testIncRegisterInvalidContextImpl)

testDecRegisterImpl :: Bool
testDecRegisterImpl =
  regGet context EAX == Valid (-1)
  where
    context = regDec (Valid newContext) EAX

testDecRegister :: Test
testDecRegister = TestCase (assertBool "dec register" testDecRegisterImpl)

testDecRegisterInvalidContextImpl :: Bool
testDecRegisterInvalidContextImpl =
  case regDec (Invalid "err") EAX of
    (Invalid _) -> True
    _ -> False

testDecRegisterInvalidContext :: Test
testDecRegisterInvalidContext = TestCase (assertBool "dec register invalid context" testDecRegisterInvalidContextImpl)

testAddRegisterImpl :: Bool
testAddRegisterImpl =
  regGet context EAX == Valid 3
  where
    context = regAdd (Valid newContext) EAX 3

testAddRegister :: Test
testAddRegister = TestCase (assertBool "add register" testAddRegisterImpl)

testAddRegisterInvalidContextImpl :: Bool
testAddRegisterInvalidContextImpl =
  case regAdd (Invalid "err") EAX 3 of
    (Invalid _) -> True
    _ -> False

testAddRegisterInvalidContext :: Test
testAddRegisterInvalidContext = TestCase (assertBool "add register invalid context" testAddRegisterInvalidContextImpl)

testSubRegisterImpl :: Bool
testSubRegisterImpl =
  regGet context EAX == Valid (-3)
  where
    context = regSub (Valid newContext) EAX 3

testSubRegister :: Test
testSubRegister = TestCase (assertBool "sub register" testSubRegisterImpl)

testSubRegisterInvalidContextImpl :: Bool
testSubRegisterInvalidContextImpl =
  case regSub (Invalid "err") EAX 3 of
    (Invalid _) -> True
    _ -> False

testSubRegisterInvalidContext :: Test
testSubRegisterInvalidContext = TestCase (assertBool "sub register invalid context" testSubRegisterInvalidContextImpl)

testMulRegisterImpl :: Bool
testMulRegisterImpl =
  regGet context EAX == Valid 9
  where
    context = regMul (regSet (Valid newContext) EAX 3) EAX 3

testMulRegister :: Test
testMulRegister = TestCase (assertBool "mul register" testMulRegisterImpl)

testMulRegisterInvalidContextImpl :: Bool
testMulRegisterInvalidContextImpl =
  case regMul (Invalid "err") EAX 3 of
    (Invalid _) -> True
    _ -> False

testMulRegisterInvalidContext :: Test
testMulRegisterInvalidContext = TestCase (assertBool "mul register invalid context" testMulRegisterInvalidContextImpl)

testDivRegisterImpl :: Bool
testDivRegisterImpl =
  regGet context EAX == Valid 3
  where
    context = regDiv (regSet (Valid newContext) EAX 9) EAX 3

testDivRegister :: Test
testDivRegister = TestCase (assertBool "div register" testDivRegisterImpl)

testDivZeroRegisterImpl :: Bool
testDivZeroRegisterImpl =
  case regDiv (regSet (Valid newContext) EAX 9) EAX 0 of
    (Invalid _) -> True
    _ -> False

testDivZeroRegister :: Test
testDivZeroRegister = TestCase (assertBool "div zero register" testDivZeroRegisterImpl)

testDivRegisterInvalidContextImpl :: Bool
testDivRegisterInvalidContextImpl =
  case regDiv (Invalid "err") EAX 3 of
    (Invalid _) -> True
    _ -> False

testDivRegisterInvalidContext :: Test
testDivRegisterInvalidContext = TestCase (assertBool "div register invalid context" testDivRegisterInvalidContextImpl)

testModRegisterImpl :: Bool
testModRegisterImpl =
  regGet context EAX == Valid 1
  where
    context = regMod (regSet (Valid newContext) EAX 9) EAX 2

testModRegister :: Test
testModRegister = TestCase (assertBool "mod register" testModRegisterImpl)

testModZeroRegisterImpl :: Bool
testModZeroRegisterImpl =
  case regMod (regSet (Valid newContext) EAX 9) EAX 0 of
    (Invalid _) -> True
    _ -> False

testModZeroRegister :: Test
testModZeroRegister = TestCase (assertBool "mod zero register" testModZeroRegisterImpl)

testModRegisterInvalidContextImpl :: Bool
testModRegisterInvalidContextImpl =
  case regMod (Invalid "err") EAX 3 of
    (Invalid _) -> True
    _ -> False

testModRegisterInvalidContext :: Test
testModRegisterInvalidContext = TestCase (assertBool "mod register invalid context" testModRegisterInvalidContextImpl)

testAndRegisterImpl :: Bool
testAndRegisterImpl =
  regGet context EAX == Valid 1
  where
    context = regAnd (regSet (Valid newContext) EAX 5) EAX 3

testAndRegister :: Test
testAndRegister = TestCase (assertBool "and register" testAndRegisterImpl)

testAndRegisterInvalidContextImpl :: Bool
testAndRegisterInvalidContextImpl =
  case regAnd (Invalid "err") EAX 3 of
    (Invalid _) -> True
    _ -> False

testAndRegisterInvalidContext :: Test
testAndRegisterInvalidContext = TestCase (assertBool "and register invalid context" testAndRegisterInvalidContextImpl)

testOrRegisterImpl :: Bool
testOrRegisterImpl =
  regGet context EAX == Valid 7
  where
    context = regOr (regSet (Valid newContext) EAX 5) EAX 3

testOrRegister :: Test
testOrRegister = TestCase (assertBool "or register" testOrRegisterImpl)

testOrRegisterInvalidContextImpl :: Bool
testOrRegisterInvalidContextImpl =
  case regOr (Invalid "err") EAX 3 of
    (Invalid _) -> True
    _ -> False

testOrRegisterInvalidContext :: Test
testOrRegisterInvalidContext = TestCase (assertBool "or register invalid context" testOrRegisterInvalidContextImpl)

testXorRegisterImpl :: Bool
testXorRegisterImpl =
  regGet context EAX == Valid 6
  where
    context = regXor (regSet (Valid newContext) EAX 5) EAX 3

testXorRegister :: Test
testXorRegister = TestCase (assertBool "xor register" testXorRegisterImpl)

testXorRegisterInvalidContextImpl :: Bool
testXorRegisterInvalidContextImpl =
  case regXor (Invalid "err") EAX 3 of
    (Invalid _) -> True
    _ -> False

testXorRegisterInvalidContext :: Test
testXorRegisterInvalidContext = TestCase (assertBool "xor register invalid context" testXorRegisterInvalidContextImpl)

testNotRegisterImpl :: Bool
testNotRegisterImpl =
  regGet context EAX == Valid (-2)
  where
    context = regNot (regSet (Valid newContext) EAX 1) EAX

testNotRegister :: Test
testNotRegister = TestCase (assertBool "not register" testNotRegisterImpl)

testNotRegisterInvalidContextImpl :: Bool
testNotRegisterInvalidContextImpl =
  case regNot (Invalid "err") EAX of
    (Invalid _) -> True
    _ -> False

testNotRegisterInvalidContext :: Test
testNotRegisterInvalidContext = TestCase (assertBool "not register invalid context" testNotRegisterInvalidContextImpl)

testStackPushPopImpl :: Bool
testStackPushPopImpl =
  value == 3
  where
    (v, _) = case stackPop (stackPush (stackPush (Valid newContext) 2) 3) of
      (Invalid s) -> ((Invalid s), (Invalid s))
      Valid (one, Valid two) -> (Valid one, Valid two)
      Valid (_, (Invalid s)) -> (Valid 987654321, (Invalid s))
    value = fromValidState 987654321 v

testStackPushPop :: Test
testStackPushPop = TestCase (assertBool "stack push pop peek" testStackPushPopImpl)

testStackPushPopPeekImpl :: Bool
testStackPushPopPeekImpl =
  fromValidState 0 value == 2
  where
    -- caution, the code executes from the bottom to the top
    (_, c) = case stackPop (stackPush (stackPush (Valid newContext) 2) 3) of
      (Invalid s) -> ((Invalid s), (Invalid s))
      Valid (one, Valid two) -> (Valid one, Valid two)
      Valid (_, (Invalid s)) -> (Valid 987654321, (Invalid s))
    (value, _) = case stackPeek c of
      (Invalid s) -> ((Invalid s), (Invalid s))
      Valid (one, Valid two) -> (Valid one, Valid two)
      Valid (_, (Invalid s)) -> (Valid 987654321, (Invalid s))

testStackPushPopPeek :: Test
testStackPushPopPeek = TestCase (assertBool "stack push pop peek" testStackPushPopPeekImpl)

testStackDup :: Test
testStackDup = TestCase (assertBool "stack dup" testStackDupImpl)

testStackDupImpl :: Bool
testStackDupImpl =
  fromValidState 0 value == 2
  where
    -- caution, the code executes from the bottom to the top
    (_, c) = case stackPop (stackPush (stackPush (Valid newContext) 2) 3) of
      (Invalid s) -> ((Invalid s), (Invalid s))
      Valid (one, Valid two) -> (Valid one, Valid two)
      Valid (_, (Invalid s)) -> (Valid 987654321, (Invalid s))
    (value, _) = case stackPeek (stackDup c) of
      (Invalid s) -> ((Invalid s), (Invalid s))
      Valid (one, Valid two) -> (Valid one, Valid two)
      Valid (_, (Invalid s)) -> (Valid 987654321, (Invalid s))

testStackSwap :: Test
testStackSwap = TestCase (assertBool "stack swap" testStackSwapImpl)

testStackSwapImpl :: Bool
testStackSwapImpl =
  fromValidState 0 value == 3
  where
    -- caution, the code executes from the bottom to the top
    c = stackPush (stackPush (Valid newContext) 2) 3
    (value, _) = case stackPeek (stackSwap c) of
      (Invalid s) -> ((Invalid s), (Invalid s))
      Valid (one, Valid two) -> (Valid one, Valid two)
      Valid (_, (Invalid s)) -> (Valid 987654321, (Invalid s))

testStackRot :: Test
testStackRot = TestCase (assertBool "stack rot" testStackRotImpl)

testStackRotImpl :: Bool
testStackRotImpl =
  fromValidState 0 value == 4
  where
    -- caution, the code executes from the bottom to the top
    c = stackPush (stackPush (stackPush (Valid newContext) 2) 3) 4
    (value, _) = case stackPeek (stackRot c) of
      (Invalid s) -> ((Invalid s), (Invalid s))
      Valid (one, Valid two) -> (Valid one, Valid two)
      Valid (_, (Invalid s)) -> (Valid 987654321, (Invalid s))

testHeapAlloc :: Test
testHeapAlloc = TestCase (assertBool "heap set get" testHeapAllocImpl)

testAddressDoesntExist :: Test
testAddressDoesntExist = TestList
  [
    "addressDoesntExist should return True when the address doesn't exist" ~: do
      let m = Map.fromList [(1, 10), (2, 20), (3, 30)]
      let address = 4
      assertBool "Address 4 should not exist in the map" (addressDoesntExist m address),
    "addressDoesntExist should return False when the address exists" ~: do
      let m = Map.fromList [(1, 10), (2, 20), (3, 30)]
      let address = 2
      assertBool "Address 2 exists in the map" (not $ addressDoesntExist m address)
  ]

testHeapAllocImpl :: Bool
testHeapAllocImpl =
  addr == Valid 2
  where
    -- caution, the code executes from the bottom to the top
    (addr, _) = case heapAlloc c of
      (Invalid s) -> ((Invalid s), (Invalid s))
      Valid (one, Valid two) -> (Valid one, Valid two)
      Valid (_, (Invalid s)) -> (Valid 987654321, (Invalid s))
    (_, c) = case heapAlloc (Valid newContext) of
      (Invalid s) -> ((Invalid s), (Invalid s))
      Valid (one, Valid two) -> (Valid one, Valid two)
      Valid (_, (Invalid s)) -> (Valid 987654321, (Invalid s))

testHeapAllocBisImpl :: Int
testHeapAllocBisImpl =
  addr
  where
    -- caution, the code executes from the bottom to the top
    (addr, _) = case heapAlloc (Valid newContext) of
      (Invalid s) -> (-1, (Invalid s))
      Valid (one, Valid two) -> (one, Valid two)
      Valid (_, (Invalid s)) -> (-1, (Invalid s))

testHeapAllocBis :: Test
testHeapAllocBis = TestCase (assertEqual "heap alloc bis" 1 testHeapAllocBisImpl)

testHeapSetGet :: Test
testHeapSetGet = TestCase (assertBool "heap set get" testHeapSetGetImpl)

testHeapSetGetImpl :: Bool
testHeapSetGetImpl =
  value == 42
  where
    value = fromValidState 98764321 (heapGet (heapSet ctx 1 42) 1)
    ctx = case heapAlloc (Valid newContext) of
      (Invalid s) -> (Invalid s)
      Valid (_, Valid two) -> Valid two
      Valid (_, (Invalid s)) -> (Invalid s)

testLabelSetGetImpl :: Bool
testLabelSetGetImpl =
  value == Valid 42
  where
    value = labelGet c "ouioui"
    c = labelSet (Valid newContext) "ouioui" 42

testLabelSetGet :: Test
testLabelSetGet = TestCase (assertBool "label set get" testLabelSetGetImpl)

testsLabelsFuncs :: Test
testsLabelsFuncs = TestList
  [
    "Invalid labelSet" ~: labelSet (Invalid "Error") "jaja" 43 ~?= Invalid "Error",
    "Invalid labelGet" ~: labelGet (Invalid "Error") "test" ~?= Invalid "Error",
    "Invalid labelGet" ~: labelGet (Valid newContext) "test" ~?= Invalid "Label not allocated",
    "Invalid labelFree" ~: labelFree (Invalid "Error") "test" ~?= Invalid "Error",
    "Valid labelFree" ~: labelFree (labelSet (Valid newContext) "test" 1) "test" ~?= Valid newContext {labels = Labels Map.empty}
  ]

testFlagGetSetImpl :: Bool
testFlagGetSetImpl =
  value == True
  where
    value = flagGet c ZF
    c = flagSet (Valid newContext) ZF True

testFlagGetSet :: Test
testFlagGetSet = TestCase (assertBool "flag get set" testFlagGetSetImpl)

testFlagInvalid :: Test
testFlagInvalid = TestList
  [
    "Invalid flagSet" ~: flagSet (Invalid "Error") ZF True ~?= Invalid "Error",
    "Invalid flagGet" ~: flagGet (Invalid "Error") ZF ~?= False
  ]

testCodeFromValidStateInt :: Test
testCodeFromValidStateInt =
  TestList
    [ "returns the correct SyscallCode for a ValidState Int"
        ~: let x = Valid 1
            in codeFromValidStateInt x ~?= SCExit,
      "returns SCExit for an Invalid ValidState Int"
        ~: let x = Valid 42
            in codeFromValidStateInt x ~?= SCExit,
      "returns SCExit for an Invalid ValidState Int"
        ~: let x = Invalid "Error"
            in codeFromValidStateInt x ~?= SCExit,
      "returns an Invalid ValidState Context for an Invalid input"
        ~: let x = Invalid "Error"
            in callExit x ~?= Invalid "Error",
      "returns SCEasyPrint from the ValidState Int"
        ~: let x = Valid 4
            in codeFromValidStateInt x ~?= SCEasyPrint,
      "sets the exit flag to True for a Valid input"
        ~: let x = Valid newContext {exit = False}
            in callExit x ~?= Valid newContext {exit = True},
      "sets the exit flag to True for a Valid input"
        ~: let x = Valid newContext {exit = False}
               (Valid context') = callExit x
               context = exit context'
            in context ~?= True
    ]

testExecSyscall :: Test
testExecSyscall = TestList
  [
    "If context is invalid should print invalid"
      ~: do
        let (result, ioAction) = execSyscall (Invalid "Error") SCExit
        (result) ~?= Invalid "Error",
    "If context is valid and syscall is exit should print exit"
      ~: do
        let (result, ioAction) = execSyscall (Valid newContext) SCExit
        let expected = (Valid newContext {exit = True}, putStr "Exit")
        (result) ~?= Valid newContext {exit = True},
    "If context is valid and syscall is print should return valid context"
      ~: do
        let (result, ioAction) = execSyscall (Valid newContext) SCEasyPrint
        let expected = (Valid newContext, putStr "0")
        (result) ~?= Valid newContext
  ]

testExecSyscallWrapper:: Test
testExecSyscallWrapper = TestList
  [
    "If context is invalid should print invalid"
      ~: do
        let (result, ioAction) = execSyscallWrapper (Invalid "Error")
        (result) ~?= Invalid "Error",
    "If context is valid and syscall is exit should print exit"
      ~: do
        let (result, ioAction) = execSyscallWrapper (Valid newContext {exit=True, registers = Registers (Map.fromList [(EAX,0),(EBX,0),(ECX,0),(EDX,0),(ESI,0),(EDI,0),(EBP,0),(ESP,0)])})
        -- let expected = Valid newContext
        (result) ~?= Valid newContext {exit = True}
  ]

extract :: ValidState t -> t
extract (Valid x) = x
extract (Invalid _) = error "Invalid"

testCallEasyPrint :: Test
testCallEasyPrint =
  TestList
    [ "prints an error message for an Invalid input"
        ~: let x = Invalid "Error"
            in callEasyPrint x, -- should not throw an exception
      "prints a value for a Valid input"
        ~: let x = Valid newContext {registers = Registers (Map.fromList [(EAX, 42), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])}
            in callEasyPrint x,
      "prints a negative value for a Valid input"
        ~: let x = Valid newContext {registers = Registers (Map.fromList [(EAX, -42), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])}
            in callEasyPrint x -- should print "-42"
    ]

testCodeFromEAX :: Test
testCodeFromEAX =
  TestList
    [ "returns the correct SyscallCode for a Valid newContext with SCExit"
        ~: let x = newContext {registers = Registers (Map.fromList [(EAX, 1), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])}
            in codeFromEAX x ~?= SCExit,
      "returns  the correct SyscallCode for a Valid Context with SCExit and negative value"
        ~: let x = newContext {registers = Registers (Map.fromList [(EAX, -1), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])}
            in codeFromEAX x ~?= SCExit
    ]

testBlock :: Test
testBlock =
  TestList
    [ "two blocks with the same name are not equal"
        ~: let block1 = Block "foo" (Valid newContext) []
               block2 = Block "foo" (Valid newContext) []
            in block1 == block2 ~?= True,
      "two blocks with different names are not equal"
        ~: let block1 = Block "foo" (Valid newContext) []
               block2 = Block "bar" (Valid newContext) []
            in block1 == block2 ~?= False,
      "two blocks with the same name and context are equal"
        ~: let block1 = Block "foo" (Valid newContext) []
               block2 = Block "foo" (Valid newContext) []
            in block1 == block2 ~?= True
    ]

testBlockMap :: Test
testBlockMap =
  TestList
    [ "adding a block to an empty map succeeds"
        ~: let blockMap = newBlockMap
               block = Block "foo" (Valid newContext) []
               result = blockAdd (Valid newContext {blocks = blockMap}) "foo"
            in result == Valid newContext {blocks = BlockMap (Map.singleton "foo" block)} ~?= True,
      "adding a block with the same name as an existing block fails"
        ~: let blockMap = BlockMap (Map.singleton "foo" (Block "foo" (Valid newContext) []))
               result = blockAdd (Valid newContext {blocks = blockMap}) "foo"
            in result == Invalid "Block already defined: foo" ~?= True,
      "replacing an existing block succeeds"
        ~: let blockMap = BlockMap (Map.singleton "foo" (Block "foo" (Valid newContext) []))
               block = Block "foo" (Valid newContext {registers = Registers (Map.singleton EAX 42)}) []
               result = blockReplace (Valid newContext {blocks = blockMap}) (Valid block)
            in result == Valid newContext {blocks = BlockMap (Map.singleton "foo" block)} ~?= True,
      "replacing a non-existing block fails"
        ~: let blockMap = newBlockMap
               block = Block "foo" (Valid newContext) []
               result = blockReplace (Valid newContext {blocks = blockMap}) (Valid block)
            in result == Invalid "Block not found: foo" ~?= True,
      "getting an existing block succeeds"
        ~: let blockMap = BlockMap (Map.singleton "foo" (Block "foo" (Valid newContext) []))
               result = blockGet (Valid newContext {blocks = blockMap}) "foo"
            in result == Valid (Block "foo" (Valid newContext) []) ~?= True,
      "getting a non-existing block fails"
        ~: let blockMap = newBlockMap
               result = blockGet (Valid newContext {blocks = blockMap}) "foo"
            in result == Invalid "Block not found: foo" ~?= True
    ]

testBlockShow :: Test
testBlockShow =
  TestList
    [ "shows a block with no parameters correctly"
        ~: let block = Block "foo" (Valid newContext) []
            in show block ~?= "Block {blockName = \"foo\", blockContext = Valid Context {registers = Registers {regs = fromList [(EAX,0),(EBX,0),(ECX,0),(EDX,0),(ESI,0),(EDI,0),(EBP,0),(ESP,0)]}, stack = Stack {pile = []}, heap = Heap {mem = fromList []}, instructions = [\n\n], symbolTable = SymTable {symTable = []}, labels = Labels {labelMap = fromList []}, flags = Flags {flagMap = fromList [(ZF,False),(SF,False),(OF,False),(CF,False),(PF,False),(AF,False)]}, instructionPointer = 0, exit = False, uuids = 0[]BlockMap {blockMap = fromList []}}, blockParamTypes = []}",
      "shows a block with parameters correctly"
        ~: let block = Block "bar" (Valid newContext) [GInt, GBool]
            in show block ~?= "Block {blockName = \"bar\", blockContext = Valid Context {registers = Registers {regs = fromList [(EAX,0),(EBX,0),(ECX,0),(EDX,0),(ESI,0),(EDI,0),(EBP,0),(ESP,0)]}, stack = Stack {pile = []}, heap = Heap {mem = fromList []}, instructions = [\n\n], symbolTable = SymTable {symTable = []}, labels = Labels {labelMap = fromList []}, flags = Flags {flagMap = fromList [(ZF,False),(SF,False),(OF,False),(CF,False),(PF,False),(AF,False)]}, instructionPointer = 0, exit = False, uuids = 0[]BlockMap {blockMap = fromList []}}, blockParamTypes = [GInt,GBool]}"
    ]

testBlockAddInvalid :: Test
testBlockAddInvalid =
  TestList
    [ "returns the input for an Invalid context"
        ~: let c = Invalid "Error"
            in blockAdd c "foo" ~?= Invalid "Error",
      "returns an error message for a duplicate block name"
        ~: let c = Valid newContext {blocks = BlockMap (Map.singleton "foo" (Block "foo" (Valid newContext) []))}
            in blockAdd c "foo" ~?= Invalid "Block already defined: foo",
      "returns the input for an Invalid context"
        ~: let c = Invalid "Error"
               block = Block "foo" (Valid newContext) []
            in blockReplace c (Valid block) ~?= Invalid "Error",
      "returns the Invalid input for a valid  context"
        ~: let c = Valid newContext
               block = Invalid "Error"
            in blockReplace c block ~?= Invalid "Error",
      "returns an error message for a non-existing block"
        ~: let c = Valid newContext {blocks = newBlockMap}
               block = Block "foo" (Valid newContext) []
            in blockReplace c (Valid block) ~?= Invalid "Block not found: foo",
      "returns the input for an Invalid context"
        ~: let c = Invalid "Error"
            in blockGet c "foo" ~?= Invalid "Error",
      "returns an error message for a non-existing block"
        ~: let c = Valid newContext {blocks = newBlockMap}
            in blockGet c "foo" ~?= Invalid "Block not found: foo"
    ]

validOrError :: ValidState a -> a
validOrError (Valid x) = x
validOrError (Invalid s) = error s

testSymTable :: Test
testSymTable =
  TestList
    [ "creates a new empty symbol table"
        ~: symTable newSymTable ~?= [],
      "adds a new symbol to a Valid context"
        ~: let c = Valid newContext
               c' = symSet c "foo" GInt
            in symTable (symbolTable (validOrError c')) ~?= [("foo", GInt)],
      "returns an error for an Invalid context"
        ~: let c = Invalid "Error"
               c' = symSet c "foo" GInt
            in c' ~?= Invalid "Failed to set symbol because the previous error was thrown: Error",
      "returns an error for a missing symbol"
        ~: let c = Valid newContext
               r = symGet c "foo"
            in r ~?= Invalid "Symbol not found",
      "returns the index of an existing symbol"
        ~: let c = Valid newContext {symbolTable = SymTable [("foo", GInt), ("bar", GBool)]}
               r = symGet c "bar"
            in r ~?= Valid 1,
      "returns an error for an Invalid context"
        ~: let c = Invalid "Error"
               r = symGetFull c "foo"
            in r ~?= Invalid "Error",
      "returns an error for a missing symbol"
        ~: let c = Valid newContext
               r = symGetFull c "foo"
            in r ~?= Invalid "Symbol not found",
      "returns the name and type of an existing symbol"
        ~: let c = Valid newContext {symbolTable = SymTable [("foo", GInt), ("bar", GBool)]}
               r = symGetFull c "bar"
            in r ~?= Valid ("bar", GBool),
      "returns an error for an Invalid context"
        ~: let c = Invalid "Error"
               r = symGetTotalSize c
            in r ~?= Invalid "Error",
      "returns the total size of the symbol table for a Valid context"
        ~: let c = Valid newContext {symbolTable = SymTable [("foo", GInt), ("bar", GBool)]}
               r = symGetTotalSize c
            in r ~?= Valid 2
    ]

testInvalidHeap :: Test
testInvalidHeap = TestList
  [
    "Invalid Context in Heap Set" ~: heapSet (Invalid "Error") 1 4 ~?= Invalid "Error",
    "Invalid Context in Heap Get" ~: heapGet (Invalid "Error") 1 ~?= Invalid "Error",
    "Invalid Max Key" ~:
      let m = Map.fromList []
      in maxKey m ~?= (Invalid "Empty map")
  ]

testHeap :: Test
testHeap =
  TestList
    [ "heapSet sets the value of a symbol in the heap"
        ~: let c = Valid newContext {heap = newHeap {mem = Map.singleton 0 42}}
            in heapSet c 0 43 ~?= Valid (newContext {heap = newHeap {mem = Map.singleton 0 43}}),
      "heapSet returns Invalid for a negative address"
        ~: heapSet (Valid newContext) (-1) 42 ~?= Invalid "Negative address",
      "heapSet returns Invalid for an unallocated address"
        ~: let c = Valid newContext {heap = newHeap {mem = Map.singleton 0 42}}
            in heapSet c 1 43 ~?= Invalid "Address not allocated: 1",
      "heapGet gets the value of a symbol in the heap"
        ~: let c = Valid newContext {heap = newHeap {mem = Map.singleton 0 42}}
            in heapGet c 0 ~?= Valid 42,
      "heapGet returns Invalid for an unallocated address"
        ~: let c = Valid newContext {heap = newHeap {mem = Map.singleton 0 42}}
            in heapGet c 1 ~?= Invalid "Address not allocated",
      "mapKeys returns the maximum key of the map"
        ~: let m = Map.fromList [(0, 1), (1, 2), (2, 3)]
            in Map.mapKeys id m ~?= Map.fromList [(0, 1), (1, 2), (2, 3)],
      "mapKeys returns an empty map for an empty map"
        ~: Map.mapKeys id (Map.empty :: Map.Map Int Int) ~?= Map.empty,
      "heapAlloc allocates a new symbol in the heap"
        ~: let c = Valid newContext {heap = newHeap {mem = Map.singleton 0 42}}
            in heapAlloc c ~?= Valid (1, Valid newContext {heap = newHeap {mem = Map.fromList [(0, 42), (1, 0)]}}),
      "heapAlloc returns Invalid for an Invalid context"
        ~: heapAlloc (Invalid "Error") ~?= Invalid "Error",
      "heapAllocRange allocates a range in the memory"
        ~: let c = Valid newContext {heap = newHeap {mem = Map.singleton 0 42}}
            in heapAllocRange c 3 ~?= (1, Valid newContext {heap = newHeap {mem = Map.fromList [(0, 42), (1, 0), (2, 0), (3, 0)]}}),
      "heapAllocRange returns Invalid for an Invalid context"
        ~: heapAllocRange (Invalid "Error") 3 ~?= (0, Invalid "Error"),
      "heapFree frees a symbol in the heap"
        ~: let c = Valid newContext {heap = newHeap {mem = Map.singleton 0 42}}
            in heapFree c 0 ~?= Valid newContext {heap = newHeap {mem = Map.empty}},
      "heapFree returns Invalid for an Invalid context"
        ~: heapFree (Invalid "Error") 0 ~?= Invalid "Error",
      "Returns a string representation of the heap"
        ~: show (Heap (Map.fromList [(0, 1), (1, 42)])) ~?= "Heap {mem = fromList [(0,1),(1,42)]}",
      "Returns true for equal heaps"
        ~: Heap (Map.fromList [(0, 1), (1, 42)]) == Heap (Map.fromList [(0, 1), (1, 42)]) ~?= True,
      "Returns false for unequal heaps"
        ~: Heap (Map.fromList [(0, 1), (1, 42)]) == Heap (Map.fromList [(0, 1), (1, 99)]) ~?= False
    ]

testStackPush :: Test
testStackPush =
  TestList
    [ "Pushes a value on an empty stack"
        ~: stackPush (Valid newContext {stack = newStack}) 42
        ~?= Valid newContext {stack = Stack [42], registers = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 1)])},
      "Pushes a value on a non-empty stack"
        ~: stackPush (Valid newContext {stack = Stack [1, 2, 3]}) 42
        ~?= Valid newContext {stack = Stack [1, 2, 3, 42], registers = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 1)])},
      "Returns an error for an invalid context"
        ~: stackPush (Invalid "Invalid context") 42 ~?= Invalid "Invalid context"
    ]

testStackPop :: Test
testStackPop =
  TestList
    [ "Pops a value from a non-empty stack"
        ~: stackPop (Valid newContext {stack = Stack [1, 2, 3]})
        ~?= Valid (3, Valid newContext {stack = Stack [1, 2], registers = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, -1)])}),
      "Returns an error for an empty stack"
        ~: stackPop (Valid newContext {stack = newStack}) ~?= Invalid "Empty stack",
      "Returns an error for an invalid context"
        ~: stackPop (Invalid "Invalid context") ~?= Invalid "Invalid context"
    ]

testStackPeek :: Test
testStackPeek =
  TestList
    [ "Peeks a value from a non-empty stack"
        ~: stackPeek (Valid newContext {stack = Stack [1, 2, 3]})
        ~?= Valid (1, Valid newContext {stack = Stack [1, 2, 3], registers = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])}),
      "Returns an error for an empty stack"
        ~: stackPeek (Valid newContext {stack = newStack}) ~?= Invalid "Empty stack",
      "Returns an error for an invalid context"
        ~: stackPeek (Invalid "Invalid context") ~?= Invalid "Invalid context",
      "Returns a string representation of the stack"
        ~: show (Stack [1, 2, 3]) ~?= "Stack {pile = [1,2,3]}",
      "Returns true for equal stacks"
        ~: Stack [1, 2, 3] == Stack [1, 2, 3] ~?= True,
      "Returns false for unequal stacks"
        ~: Stack [1, 2, 3] == Stack [1, 2, 99] ~?= False
    ]

testStackDup2 :: Test
testStackDup2 =
  TestList
    [ "Duplicates the top value of a non-empty stack"
        ~: stackDup (Valid newContext {stack = Stack [1, 2, 3]})
        ~?= Valid newContext {stack = Stack [1, 1, 2, 3], registers = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])},
      "Returns an error for an empty stack"
        ~: stackDup (Valid newContext {stack = newStack}) ~?= Invalid "Empty stack",
      "Returns an error for an invalid context"
        ~: stackDup (Invalid "Invalid context") ~?= Invalid "Invalid context"
    ]

testStackSwap2 :: Test
testStackSwap2 =
  TestList
    [ "Swaps the two top values of a non-empty stack"
        ~: stackSwap (Valid newContext {stack = Stack [1, 2, 3]})
        ~?= Valid newContext {stack = Stack [2, 1, 3], registers = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])},
      "Returns an error for a stack with less than two values"
        ~: stackSwap (Valid newContext {stack = Stack [1]}) ~?= Invalid "Not enough values on the stack",
      "Returns an error for an empty stack"
        ~: stackSwap (Valid newContext {stack = newStack}) ~?= Invalid "Empty stack",
      "Returns an error for an invalid context"
        ~: stackSwap (Invalid "Invalid context") ~?= Invalid "Invalid context"
    ]

testStackRot2 :: Test
testStackRot2 =
  TestList
    [ "Rotates the three top values of a non-empty stack"
        ~: stackRot (Valid newContext {stack = Stack [1, 2, 3, 4]})
        ~?= Valid newContext {stack = Stack [3, 1, 2, 4], registers = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])},
      "Returns an error for a stack with less than three values"
        ~: stackRot (Valid newContext {stack = Stack [1, 2]}) ~?= Invalid "Not enough values on the stack",
      "Returns an error for a stack with less than three values"
        ~: stackRot (Valid newContext {stack = Stack [1]}) ~?= Invalid "Not enough values on the stack",
      "Returns an error for an empty stack"
        ~: stackRot (Valid newContext {stack = newStack}) ~?= Invalid "Empty stack",
      "Returns an error for an invalid context"
        ~: stackRot (Invalid "Invalid context") ~?= Invalid "Invalid context"
    ]

testStackGetPointer :: Test
testStackGetPointer =
  TestList
    [ "Returns the length of a non-empty stack"
        ~: stackGetPointer (Valid newContext {stack = Stack [1, 2, 3]}) ~?= (3, Valid newContext {stack = Stack [1, 2, 3], registers = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])}),
      "Returns 0 for an empty stack"
        ~: stackGetPointer (Valid newContext {stack = newStack}) ~?= (0, Valid newContext {stack = newStack, registers = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])}),
      "Returns 0 for an invalid context"
        ~: stackGetPointer (Invalid "Invalid context") ~?= (0, Invalid "Invalid context")
    ]

testStackClear :: Test
testStackClear = TestList
  [
    "Clears a non-empty stack"
      ~: stackClear (Valid newContext {stack = Stack [1, 2, 3]})
      ~?= Valid newContext {stack = newStack, registers = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])},
    "Returns an error for an invalid context"
      ~: stackClear (Invalid "Invalid context") ~?= Invalid "Invalid context"
  ]

testStackGetValueFromIndex :: Test
testStackGetValueFromIndex = TestList
  [
    "Returns an error for an invalid context"
      ~: stackGetValueFromIndex (Invalid "Invalid context") 0 ~?= Invalid "Invalid context",
    "Returns an error for an empty stack"
      ~: stackGetValueFromIndex (Valid newContext {stack = newStack}) 0 ~?= Invalid "Empty stack",
    "Get the position of the elem"
      ~: stackGetValueFromIndex (Valid newContext {stack = Stack [1, 2, 3]}) 3 ~?= Valid 2,
    "Returns an error for an invalid index"
      ~: stackGetValueFromIndex (Valid newContext {stack = Stack [1, 2, 3]}) 4 ~?= Invalid "Value not retrieve"
  ]

testParam :: Test
testParam =
  TestList
    [ "Registers are equal"
        ~: Reg EAX ~?= Reg EAX,
      "Registers are not equal"
        ~: Reg EAX /= Reg EBX ~?= True,
      "Immediate values are equal"
        ~: Immediate 42 ~?= Immediate 42,
      "Immediate values are not equal"
        ~: Immediate 42 /= Immediate 99 ~?= True,
      "Memory addresses are equal"
        ~: Memory 0 ~?= Memory 0,
      "Memory addresses are not equal"
        ~: Memory 0 /= Memory 1 ~?= True,
      "Symbols are equal"
        ~: Symbol "foo" ~?= Symbol "foo",
      "Symbols are not equal"
        ~: Symbol "foo" /= Symbol "bar" ~?= True
    ]

testInstruction :: Test
testInstruction =
  TestList
    [ "Mov instructions are equal"
        ~: Mov (Reg EAX) (Immediate 42) ~?= Mov (Reg EAX) (Immediate 42),
      "Mov instructions are not equal"
        ~: Mov (Reg EAX) (Immediate 42) /= Mov (Reg EBX) (Immediate 99) ~?= True,
      "Registers are not equal to immediates"
        ~: Reg EAX /= Immediate 42 ~?= True,
      "Registers are not equal to memory addresses"
        ~: Reg EAX /= Memory 0 ~?= True,
      "Registers are not equal to symbols"
        ~: Reg EAX /= Symbol "foo" ~?= True,
      "Immediates are not equal to memory addresses"
        ~: Immediate 42 /= Memory 0 ~?= True,
      "Immediates are not equal to symbols"
        ~: Immediate 42 /= Symbol "foo" ~?= True,
      "Memory addresses are not equal to symbols"
        ~: Memory 0 /= Symbol "foo" ~?= True
    ]

testGetTrueValueFromParam :: Test
testGetTrueValueFromParam =
  TestList
    [ "Returns the value of a register"
        ~: getTrueValueFromParam (Valid newContext {registers = Registers (Map.singleton EAX 42)}) (Reg EAX) ~?= Valid 42,
      "Returns the value of an immediate"
        ~: getTrueValueFromParam (Valid newContext) (Immediate 42) ~?= Valid 42,
      "Returns the value at a memory address"
        ~: getTrueValueFromParam (Valid newContext {heap = Heap (Map.singleton 0 42)}) (Memory 0) ~?= Valid 42,
      -- "Returns the value of a symbol"
        -- ~: getTrueValueFromParam (Valid newContext {heap = Heap (Map.singleton 0 42), symbolTable = Symbol (Map.singleton "foo" 0)}) (Symbol "foo") ~?= Valid 42,
      "Returns an error for an invalid context"
        ~: getTrueValueFromParam (Invalid "Invalid context") (Reg EAX) ~?= Invalid "Invalid context"
        -- "Returns an error for an immediate type parameter"
        --   ~: getTrueValueFromParam (Valid newContext) (Immediate 42) >>= \_ -> Invalid "Cannot set value of an immediate type parameter" ~?= Invalid "Cannot set value of an immediate type parameter"
    ]

testSetTrueValueFromParam :: Test
testSetTrueValueFromParam =
  TestList
    [
      -- "Sets the value of a register"
        -- ~: setTrueValueFromParam (Valid newContext) (Reg EAX) 42 Prelude.>>= \context -> regGet (Valid context) EAX ~?= Valid 42,
      "Returns an error for an immediate type parameter"
        ~: setTrueValueFromParam (Valid newContext) (Immediate 42) 99 ~?= Invalid "Cannot set value of an immediate type parameter",
      -- "Sets the value at a memory address"
      --   ~: setTrueValueFromParam (Valid newContext {heap = Heap (Map.singleton 0 42)}) (Memory 0) 99 >>= \context -> heapGet (Valid context) 0 ~?= Valid 99,
      -- "Sets the value of a symbol"
      --   ~: setTrueValueFromParam (Valid newContext {symbolTable = Symbol (Map.singleton "foo" 0), heap = Heap (Map.singleton 0 42)}) (Symbol "foo") 99 >>= \context -> heapGet (Valid context) 0 ~?= Valid 99,
      "Returns an error for an invalid context"
        ~: setTrueValueFromParam (Invalid "Invalid context") (Reg EAX) 42 ~?= Invalid "Invalid context"
    ]

testEq :: Test
testEq =
  TestList
    [ "Registers are equal"
        ~: Reg EAX == Reg EAX ~?= True,
      "Registers are not equal"
        ~: Reg EAX == Reg EBX ~?= False,
      "Immediate values are equal"
        ~: Immediate 42 == Immediate 42 ~?= True,
      "Immediate values are not equal"
        ~: Immediate 42 == Immediate 99 ~?= False,
      "Memory addresses are equal"
        ~: Memory 0 == Memory 0 ~?= True,
      "Memory addresses are not equal"
        ~: Memory 0 == Memory 1 ~?= False,
      "Symbols are equal"
        ~: Symbol "foo" == Symbol "foo" ~?= True,
      "Symbols are not equal"
        ~: Symbol "foo" == Symbol "bar" ~?= False
    ]

testShow :: Test
testShow =
  TestList
    [ "Shows a register"
        ~: show (Reg EAX) ~?= "Reg EAX",
      "Shows an immediate"
        ~: show (Immediate 42) ~?= "Immediate 42",
      "Shows a memory address"
        ~: show (Memory 0) ~?= "Memory 0",
      "Shows a symbol"
        ~: show (Symbol "foo") ~?= "Symbol \"foo\""
    ]

testOrd :: Test
testOrd =
  TestList
    [ "Registers are equal"
        ~: Reg EAX `compare` Reg EAX ~?= EQ,
      "Registers are not equal"
        ~: Reg EAX `compare` Reg EBX ~?= LT,
      "Immediate values are equal"
        ~: Immediate 42 `compare` Immediate 42 ~?= EQ,
      "Immediate values are not equal"
        ~: Immediate 42 `compare` Immediate 99 ~?= LT,
      "Memory addresses are equal"
        ~: Memory 0 `compare` Memory 0 ~?= EQ,
      "Memory addresses are not equal"
        ~: Memory 0 `compare` Memory 1 ~?= LT,
      "Symbols are equal"
        ~: Symbol "foo" `compare` Symbol "foo" ~?= EQ,
      "Symbols are not equal"
        ~: Symbol "foo" `compare` Symbol "bar" ~?= GT
    ]

testSymGet :: Test
testSymGet = TestList
  [
    "Invalid SymGet" ~: symGet (Invalid "Error") "jsjs" ~?= Invalid "Error",
    "Invalid symGetType" ~: symGetType (Invalid "Error") "jsjsj" ~?= Invalid "Error",
    "Invalid symGetType Not found" ~: symGetType (Valid newContext {symbolTable = SymTable [("foo", GInt), ("bar", GBool)]}) "test" ~?= Invalid "Symbol not found",
    "Valid symGetType Found" ~: symGetType (Valid newContext {symbolTable = SymTable [("foo", GInt), ("bar", GBool)]}) "foo" ~?= Valid GInt
  ]

testAdaptValueToVarType :: Test
testAdaptValueToVarType = TestList
  [
    "Invalid adaptValueToVarType" ~: adaptValueToVarType GInt (Invalid "Error") ~?= "Error",
    "Valid adaptValueToVarType" ~: adaptValueToVarType GInt (Valid 42) ~?= "42",
    "Valid adaptValueToVarType" ~: adaptValueToVarType GBool (Valid 1) ~?= "true",
    "Valid adaptValueToVarType" ~: adaptValueToVarType GBool (Valid 0) ~?= "false",
    "Valid adaptValueToVarType" ~: adaptValueToVarType GVoid (Valid 42) ~?= ""
  ]

testSysPrintValue :: Test
testSysPrintValue = TestList
  [
    "Invalid sysPrintValue" ~: sysPrintValue (Invalid "Error") (Valid GInt) "test",
    "Valid sysPrintValue" ~: sysPrintValue (Valid newContext) (Invalid "Err")  "test",
    "Valid sysPrintValue" ~: sysPrintValue (Valid newContext) (Valid GInt) "test",
    "Invalid truePrintValue" ~: truePrintValue (Invalid "Error") (Reg EAX) (Reg EAX)
  ]

testContext :: Test
testContext = TestList
  [
    "Invalid saveContext" ~: saveContext (Invalid "Error") "test",
    "Valid saveContext" ~: saveContext (Valid newContext)  "test.gld",
    "showInstructArray" ~: showInstructArray [Nop] ~?= "Nop\n"
  ]

testNextUUIDValid :: Test
testNextUUIDValid = TestList
  [
    "Invalid nextUUIDValid" ~: nextUUIDValid (Invalid "Error") ~?=  (0, newContext),
    "Valid nextUUIDValid" ~: nextUUIDValid (Valid newContext) ~?= (uuids newContext, newContext {uuids = uuids newContext + 1})
  ]

testIps :: Test
testIps = TestList
  [
    "Invalid IpSet" ~: ipSet (Invalid "Error") 0 ~?= Invalid "Error",
    "Invalid IpSet Value" ~: ipSet (Valid newContext) (-1) ~?= Invalid "Invalid instruction pointer value",
    "Invalid IpGet" ~: ipGet (Invalid "Error") ~?= Invalid "Error",
    "Invalid IpInc" ~: ipInc (Invalid "Error") ~?= Invalid "Error",
    "Invalid IpInc" ~: ipInc (Valid newContext {instructions = [], instructionPointer = 1})~?= Invalid "Invalid instruction pointer value",
    "Invalid insPush" ~: insPush (Invalid "Error") Nop ~?= Invalid "Error"
  ]

testHasmNStackPush :: Test
testHasmNStackPush = TestList
  [
    "hasmNStackPush" ~: hasmNStackPush 1 ~?= [Push (Immediate 0)],
    "Invalid blockInitAllocVarSpace" ~: blockInitAllocVarSpace (Invalid "Error") ~?= []
  ]

testLabels :: Test
testLabels = TestList
  [
    "Invalid parseLabels" ~: parseLabels (Invalid "Error") [] 0 ~?= Invalid "Error",
    "Empty parseLabels" ~: parseLabels (Valid newContext) [] 0 ~?= Valid newContext,
    "Valid parseLabels" ~: parseLabels (Valid newContext) [Nop] 0 ~?= Valid newContext,
    "Invalid detectLabels" ~: detectLabels (Invalid "Error") ~?= Invalid "Error",
    "Valid detectLabels" ~: detectLabels (Valid newContext) ~?= Valid newContext
  ]

testLoadContext :: Test
testLoadContext = TestCase $
    do
        c <- loadContext "tests/testLoadContext.test"
        assertEqual "loaded ok" c (Valid newContext {instructions = [
            Enter,
            Xor (Reg EAX) (Reg EAX),
            Mov (Reg EAX) (Immediate 1)]})

testBlockInitAllocVarSpace :: Test
testBlockInitAllocVarSpace = TestList [
    "Invalid blockInitAllocVarSpace" ~: blockInitAllocVarSpace (Invalid "Error") ~?= [],
    "Valid blockInitAllocVarSpace" ~: blockInitAllocVarSpace (Valid newContext) ~?= [Enter]
  ]

testParseLabels :: Test
testParseLabels = TestList [
    "Invalid parseLabels" ~: parseLabels (Invalid "Error") [] 0 ~?= Invalid "Error",
    "Empty parseLabels" ~: parseLabels (Valid newContext) [] 0 ~?= Valid newContext,
    "Valid parseLabels" ~: parseLabels (Valid newContext) [VM.Label "bob" 1, Nop, VM.Label "bobberson" 2] 0 ~?= Valid newContext {labels = Labels (Map.fromList [("bob", 0), ("bobberson", 2)])}
  ]

testSetTruValueFromParam2 :: Test
testSetTruValueFromParam2 = TestList [
    "From memory address (heap)" ~: let ctx = setTrueValueFromParam (Valid newContext {heap = Heap (Map.singleton 0 42)}) (Memory 0) 99 in heapGet ctx 0 ~?= Valid 99,
    "From symbol" ~: let ctx = setTrueValueFromParam (Valid newContext {symbolTable = SymTable [("foo", GInt), ("bar", GBool)], heap = Heap (Map.singleton 0 42)}) (Symbol "foo") 99 in heapGet ctx 0 ~?= Valid 99,
    "From invalid symbol" ~: let ctx = setTrueValueFromParam (Valid newContext {symbolTable = SymTable [("foo", GInt), ("bar", GBool)], heap = Heap (Map.singleton 0 42)}) (Symbol "quiche") 99 in heapGet ctx 0 ~?= Invalid "Symbol not found"
  ]

testSyscallCodeShow :: Test
testSyscallCodeShow = TestList
  [ "SCExit should show as \"SCExit\"" ~: do
        assertEqual "show SCExit should be \"SCExit\"" (show SCExit) "SCExit"
  , "SCEasyPrint should show as \"SCEasyPrint\"" ~: do
        assertEqual "show SCEasyPrint should be \"SCEasyPrint\"" (show SCEasyPrint) "SCEasyPrint"
  ]

testRegistersEq :: Test
testRegistersEq = TestList
  [ "Registers should be equal to itself" ~: do
        let regs = Registers (Map.fromList [(EAX, 0x1000), (EBX, 0x2000)])
        assertEqual "regs should be equal to itself" regs regs
  , "Registers should be equal to an equal value" ~: do
        let regs1 = Registers (Map.fromList [(EAX, 0x1000), (EBX, 0x2000)])
            regs2 = Registers (Map.fromList [(EAX, 0x1000), (EBX, 0x2000)])
        assertEqual "regs1 should be equal to regs2" regs1 regs2
  , "Registers should not be equal to a different value" ~: do
        let regs1 = Registers (Map.fromList [(EAX, 0x1000), (EBX, 0x2000)])
            regs2 = Registers (Map.fromList [(EAX, 0x1000), (EBX, 0x3000)])
        assertBool "regs1 should not be equal to regs2" (regs1 /= regs2)
  ]

testStackEq :: Test
testStackEq = TestList
  [ "Stack should be equal to itself" ~: do
        let stack = Stack [1, 2, 3]
        assertEqual "stack should be equal to itself" stack stack
  , "Stack should be equal to an equal value" ~: do
        let stack1 = Stack [1, 2, 3]
            stack2 = Stack [1, 2, 3]
        assertEqual "stack1 should be equal to stack2" stack1 stack2
  , "Stack should not be equal to a different value" ~: do
        let stack1 = Stack [1, 2, 3]
            stack2 = Stack [1, 2, 4]
        assertBool "stack1 should not be equal to stack2" (stack1 /= stack2)
  ]

testHeapEq :: Test
testHeapEq = TestList
  [ "Heap should be equal to itself" ~: do
        let heap = Heap (Map.fromList [(0x1000, 0x1234), (0x2000, 0x5678)])
        assertEqual "heap should be equal to itself" heap heap
  , "Heap should be equal to an equal value" ~: do
        let heap1 = Heap (Map.fromList [(0x1000, 0x1234), (0x2000, 0x5678)])
            heap2 = Heap (Map.fromList [(0x1000, 0x1234), (0x2000, 0x5678)])
        assertEqual "heap1 should be equal to heap2" heap1 heap2
  , "Heap should not be equal to a different value" ~: do
        let heap1 = Heap (Map.fromList [(0x1000, 0x1234), (0x2000, 0x5678)])
            heap2 = Heap (Map.fromList [(0x1000, 0x1234), (0x2000, 0x5679)])
        assertBool "heap1 should not be equal to heap2" (heap1 /= heap2)
  ]


testSymTableEq :: Test
testSymTableEq = TestList
  [ "SymTable should be equal to itself" ~: do
        let symTable = SymTable [("x", GInt), ("y", GBool)]
        assertEqual "symTable should be equal to itself" symTable symTable
  , "SymTable should be equal to an equal value" ~: do
        let symTable1 = SymTable [("x", GInt), ("y", GBool)]
            symTable2 = SymTable [("x", GInt), ("y", GBool)]
        assertEqual "symTable1 should be equal to symTable2" symTable1 symTable2
  , "SymTable should not be equal to a different value" ~: do
        let symTable1 = SymTable [("x", GInt), ("y", GBool)]
            symTable2 = SymTable [("x", GInt), ("y", GVoid)]
        assertBool "symTable1 should not be equal to symTable2" (symTable1 /= symTable2)
  ]

testLabelsEq :: Test
testLabelsEq = TestList
  [ "Labels should be equal to itself" ~: do
        let labels = Labels (Map.fromList [("label1", 0x1000), ("label2", 0x2000)])
        assertEqual "labels should be equal to itself" labels labels
  , "Labels should be equal to an equal value" ~: do
        let labels1 = Labels (Map.fromList [("label1", 0x1000), ("label2", 0x2000)])
            labels2 = Labels (Map.fromList [("label1", 0x1000), ("label2", 0x2000)])
        assertEqual "labels1 should be equal to labels2" labels1 labels2
  , "Labels should not be equal to a different value" ~: do
        let labels1 = Labels (Map.fromList [("label1", 0x1000), ("label2", 0x2000)])
            labels2 = Labels (Map.fromList [("label1", 0x1000), ("label2", 0x3000)])
        assertBool "labels1 should not be equal to labels2" (labels1 /= labels2)
  ]

testFlagsEq :: Test
testFlagsEq = TestList
  [ "Flags should be equal to itself" ~: do
        let flags = Flags (Map.fromList [(ZF, True), (CF, False)])
        assertEqual "flags should be equal to itself" flags flags
  , "Flags should be equal to an equal value" ~: do
        let flags1 = Flags (Map.fromList [(ZF, True), (CF, False)])
            flags2 = Flags (Map.fromList [(ZF, True), (CF, False)])
        assertEqual "flags1 should be equal to flags2" flags1 flags2
  , "Flags should not be equal to a different value" ~: do
        let flags1 = Flags (Map.fromList [(ZF, True), (CF, False)])
            flags2 = Flags (Map.fromList [(ZF, False), (CF, False)])
        assertBool "flags1 should not be equal to flags2" (flags1 /= flags2)
  ]

testBlockMapEq :: Test
testBlockMapEq = TestList
  [ "BlockMap should be equal to itself" ~: do
        let blockMap = BlockMap (Map.fromList [("block1", Block "foo" (Valid newContext) []), ("block2", Block "foo2" (Valid newContext) [])])
        assertEqual "blockMap should be equal to itself" blockMap blockMap
  , "BlockMap should be equal to an equal value" ~: do
        let blockMap1 = BlockMap (Map.fromList [("block1", Block "foo" (Valid newContext) []), ("block2", Block "foo2" (Valid newContext) [])])
            blockMap2 = BlockMap (Map.fromList [("block1", Block "foo" (Valid newContext) []), ("block2", Block "foo2" (Valid newContext) [])])
        assertEqual "blockMap1 should be equal to blockMap2" blockMap1 blockMap2
  , "BlockMap should not be equal to a different value" ~: do
        let blockMap1 = BlockMap (Map.fromList [("block1", Block "foo" (Valid newContext) []), ("block2", Block "foo2" (Valid newContext) [])])
            blockMap2 = BlockMap (Map.fromList [("block1", Block "foo" (Valid newContext) []), ("block2", Block "foo2" (Valid newContext) [GInt])])
        assertBool "blockMap1 should not be equal to blockMap2" (blockMap1 /= blockMap2)
  ]

functionalVMTests :: Test
functionalVMTests =
  TestList [
    testSyscallCodeShow,
    testRegistersEq,
    testStackEq,
    testHeapEq,
    testSymTableEq,
    testLabelsEq,
    testFlagsEq,
    testBlockMapEq
  ]

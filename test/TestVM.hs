module TestVM (
    testIncRegister,
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
    testOrRegisterInvalidContext,
    testXorRegister,
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
    testFlagGetSet
) where

import Test.HUnit
import VM
import ValidState

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

testFlagGetSetImpl :: Bool
testFlagGetSetImpl =
  value == True
  where
    value = flagGet c ZF
    c = flagSet (Valid newContext) ZF True

testFlagGetSet :: Test
testFlagGetSet = TestCase (assertBool "flag get set" testFlagGetSetImpl)

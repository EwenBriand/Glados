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
import qualified Data.Maybe as Data


testIncRegisterImpl :: Bool
testIncRegisterImpl =
  regGet context EAX == Just 1
  where
    context = regInc (Just newContext) EAX

testIncRegister :: Test
testIncRegister = TestCase (assertBool "inc register" testIncRegisterImpl)

testIncRegisterInvalidContextImpl :: Bool
testIncRegisterInvalidContextImpl =
  case regInc Nothing EAX of
    Nothing -> True
    _ -> False

testIncRegisterInvalidContext :: Test
testIncRegisterInvalidContext = TestCase (assertBool "inc register invalid context" testIncRegisterInvalidContextImpl)

testDecRegisterImpl :: Bool
testDecRegisterImpl =
  regGet context EAX == Just (-1)
  where
    context = regDec (Just newContext) EAX

testDecRegister :: Test
testDecRegister = TestCase (assertBool "dec register" testDecRegisterImpl)

testDecRegisterInvalidContextImpl :: Bool
testDecRegisterInvalidContextImpl =
  case regDec Nothing EAX of
    Nothing -> True
    _ -> False

testDecRegisterInvalidContext :: Test
testDecRegisterInvalidContext = TestCase (assertBool "dec register invalid context" testDecRegisterInvalidContextImpl)

testAddRegisterImpl :: Bool
testAddRegisterImpl =
  regGet context EAX == Just 3
  where
    context = regAdd (Just newContext) EAX 3

testAddRegister :: Test
testAddRegister = TestCase (assertBool "add register" testAddRegisterImpl)

testAddRegisterInvalidContextImpl :: Bool
testAddRegisterInvalidContextImpl =
  case regAdd Nothing EAX 3 of
    Nothing -> True
    _ -> False

testAddRegisterInvalidContext :: Test
testAddRegisterInvalidContext = TestCase (assertBool "add register invalid context" testAddRegisterInvalidContextImpl)

testSubRegisterImpl :: Bool
testSubRegisterImpl =
  regGet context EAX == Just (-3)
  where
    context = regSub (Just newContext) EAX 3

testSubRegister :: Test
testSubRegister = TestCase (assertBool "sub register" testSubRegisterImpl)

testSubRegisterInvalidContextImpl :: Bool
testSubRegisterInvalidContextImpl =
  case regSub Nothing EAX 3 of
    Nothing -> True
    _ -> False

testSubRegisterInvalidContext :: Test
testSubRegisterInvalidContext = TestCase (assertBool "sub register invalid context" testSubRegisterInvalidContextImpl)

testMulRegisterImpl :: Bool
testMulRegisterImpl =
  regGet context EAX == Just 9
  where
    context = regMul (regSet (Just newContext) EAX 3) EAX 3

testMulRegister :: Test
testMulRegister = TestCase (assertBool "mul register" testMulRegisterImpl)

testMulRegisterInvalidContextImpl :: Bool
testMulRegisterInvalidContextImpl =
  case regMul Nothing EAX 3 of
    Nothing -> True
    _ -> False

testMulRegisterInvalidContext :: Test
testMulRegisterInvalidContext = TestCase (assertBool "mul register invalid context" testMulRegisterInvalidContextImpl)

testDivRegisterImpl :: Bool
testDivRegisterImpl =
  regGet context EAX == Just 3
  where
    context = regDiv (regSet (Just newContext) EAX 9) EAX 3

testDivRegister :: Test
testDivRegister = TestCase (assertBool "div register" testDivRegisterImpl)

testDivZeroRegisterImpl :: Bool
testDivZeroRegisterImpl =
  case regDiv (regSet (Just newContext) EAX 9) EAX 0 of
    Nothing -> True
    _ -> False

testDivZeroRegister :: Test
testDivZeroRegister = TestCase (assertBool "div zero register" testDivZeroRegisterImpl)

testDivRegisterInvalidContextImpl :: Bool
testDivRegisterInvalidContextImpl =
  case regDiv Nothing EAX 3 of
    Nothing -> True
    _ -> False

testDivRegisterInvalidContext :: Test
testDivRegisterInvalidContext = TestCase (assertBool "div register invalid context" testDivRegisterInvalidContextImpl)

testModRegisterImpl :: Bool
testModRegisterImpl =
  regGet context EAX == Just 1
  where
    context = regMod (regSet (Just newContext) EAX 9) EAX 2

testModRegister :: Test
testModRegister = TestCase (assertBool "mod register" testModRegisterImpl)

testModZeroRegisterImpl :: Bool
testModZeroRegisterImpl =
  case regMod (regSet (Just newContext) EAX 9) EAX 0 of
    Nothing -> True
    _ -> False

testModZeroRegister :: Test
testModZeroRegister = TestCase (assertBool "mod zero register" testModZeroRegisterImpl)

testModRegisterInvalidContextImpl :: Bool
testModRegisterInvalidContextImpl =
  case regMod Nothing EAX 3 of
    Nothing -> True
    _ -> False

testModRegisterInvalidContext :: Test
testModRegisterInvalidContext = TestCase (assertBool "mod register invalid context" testModRegisterInvalidContextImpl)

testAndRegisterImpl :: Bool
testAndRegisterImpl =
  regGet context EAX == Just 1
  where
    context = regAnd (regSet (Just newContext) EAX 5) EAX 3

testAndRegister :: Test
testAndRegister = TestCase (assertBool "and register" testAndRegisterImpl)

testAndRegisterInvalidContextImpl :: Bool
testAndRegisterInvalidContextImpl =
  case regAnd Nothing EAX 3 of
    Nothing -> True
    _ -> False

testAndRegisterInvalidContext :: Test
testAndRegisterInvalidContext = TestCase (assertBool "and register invalid context" testAndRegisterInvalidContextImpl)

testOrRegisterImpl :: Bool
testOrRegisterImpl =
  regGet context EAX == Just 7
  where
    context = regOr (regSet (Just newContext) EAX 5) EAX 3

testOrRegister :: Test
testOrRegister = TestCase (assertBool "or register" testOrRegisterImpl)

testOrRegisterInvalidContextImpl :: Bool
testOrRegisterInvalidContextImpl =
  case regOr Nothing EAX 3 of
    Nothing -> True
    _ -> False

testOrRegisterInvalidContext :: Test
testOrRegisterInvalidContext = TestCase (assertBool "or register invalid context" testOrRegisterInvalidContextImpl)

testXorRegisterImpl :: Bool
testXorRegisterImpl =
  regGet context EAX == Just 6
  where
    context = regXor (regSet (Just newContext) EAX 5) EAX 3

testXorRegister :: Test
testXorRegister = TestCase (assertBool "xor register" testXorRegisterImpl)

testXorRegisterInvalidContextImpl :: Bool
testXorRegisterInvalidContextImpl =
  case regXor Nothing EAX 3 of
    Nothing -> True
    _ -> False

testXorRegisterInvalidContext :: Test
testXorRegisterInvalidContext = TestCase (assertBool "xor register invalid context" testXorRegisterInvalidContextImpl)

testNotRegisterImpl :: Bool
testNotRegisterImpl =
  regGet context EAX == Just (-2)
  where
    context = regNot (regSet (Just newContext) EAX 1) EAX

testNotRegister :: Test
testNotRegister = TestCase (assertBool "not register" testNotRegisterImpl)

testNotRegisterInvalidContextImpl :: Bool
testNotRegisterInvalidContextImpl =
  case regNot Nothing EAX of
    Nothing -> True
    _ -> False

testNotRegisterInvalidContext :: Test
testNotRegisterInvalidContext = TestCase (assertBool "not register invalid context" testNotRegisterInvalidContextImpl)

testStackPushPopImpl :: Bool
testStackPushPopImpl =
  value == 3
  where
    (v, _) = case stackPop (stackPush (stackPush (Just newContext) 2) 3) of
      Nothing -> (Nothing, Nothing)
      Just (one, Just two) -> (Just one, Just two)
      Just (_, Nothing) -> (Just 987654321, Nothing)
    value = Data.fromMaybe 987654321 v

testStackPushPop :: Test
testStackPushPop = TestCase (assertBool "stack push pop peek" testStackPushPopImpl)

testStackPushPopPeekImpl :: Bool
testStackPushPopPeekImpl =
  Data.fromMaybe 0 value == 2
  where
    -- caution, the code executes from the bottom to the top
    (_, c) = case stackPop (stackPush (stackPush (Just newContext) 2) 3) of
      Nothing -> (Nothing, Nothing)
      Just (one, Just two) -> (Just one, Just two)
      Just (_, Nothing) -> (Just 987654321, Nothing)
    (value, _) = case stackPeek c of
      Nothing -> (Nothing, Nothing)
      Just (one, Just two) -> (Just one, Just two)
      Just (_, Nothing) -> (Just 987654321, Nothing)

testStackPushPopPeek :: Test
testStackPushPopPeek = TestCase (assertBool "stack push pop peek" testStackPushPopPeekImpl)

testStackDup :: Test
testStackDup = TestCase (assertBool "stack dup" testStackDupImpl)

testStackDupImpl :: Bool
testStackDupImpl =
  Data.fromMaybe 0 value == 2
  where
    -- caution, the code executes from the bottom to the top
    (_, c) = case stackPop (stackPush (stackPush (Just newContext) 2) 3) of
      Nothing -> (Nothing, Nothing)
      Just (one, Just two) -> (Just one, Just two)
      Just (_, Nothing) -> (Just 987654321, Nothing)
    (value, _) = case stackPeek (stackDup c) of
      Nothing -> (Nothing, Nothing)
      Just (one, Just two) -> (Just one, Just two)
      Just (_, Nothing) -> (Just 987654321, Nothing)

testStackSwap :: Test
testStackSwap = TestCase (assertBool "stack swap" testStackSwapImpl)

testStackSwapImpl :: Bool
testStackSwapImpl =
  Data.fromMaybe 0 value == 3
  where
    -- caution, the code executes from the bottom to the top
    c = stackPush (stackPush (Just newContext) 2) 3
    (value, _) = case stackPeek (stackSwap c) of
      Nothing -> (Nothing, Nothing)
      Just (one, Just two) -> (Just one, Just two)
      Just (_, Nothing) -> (Just 987654321, Nothing)

testStackRot :: Test
testStackRot = TestCase (assertBool "stack rot" testStackRotImpl)

testStackRotImpl :: Bool
testStackRotImpl =
  Data.fromMaybe 0 value == 4
  where
    -- caution, the code executes from the bottom to the top
    c = stackPush (stackPush (stackPush (Just newContext) 2) 3) 4
    (value, _) = case stackPeek (stackRot c) of
      Nothing -> (Nothing, Nothing)
      Just (one, Just two) -> (Just one, Just two)
      Just (_, Nothing) -> (Just 987654321, Nothing)

testHeapAlloc :: Test
testHeapAlloc = TestCase (assertBool "heap set get" testHeapAllocImpl)

testHeapAllocImpl :: Bool
testHeapAllocImpl =
  addr == Just 2
  where
    -- caution, the code executes from the bottom to the top
    (addr, _) = case heapAlloc c of
      Nothing -> (Nothing, Nothing)
      Just (one, Just two) -> (Just one, Just two)
      Just (_, Nothing) -> (Just 987654321, Nothing)
    (_, c) = case heapAlloc (Just newContext) of
      Nothing -> (Nothing, Nothing)
      Just (one, Just two) -> (Just one, Just two)
      Just (_, Nothing) -> (Just 987654321, Nothing)

testHeapAllocBisImpl :: Int
testHeapAllocBisImpl =
  addr
  where
    -- caution, the code executes from the bottom to the top
    (addr, _) = case heapAlloc (Just newContext) of
      Nothing -> (-1, Nothing)
      Just (one, Just two) -> (one, Just two)
      Just (_, Nothing) -> (-1, Nothing)

testHeapAllocBis :: Test
testHeapAllocBis = TestCase (assertEqual "heap alloc bis" 1 testHeapAllocBisImpl)

testHeapSetGet :: Test
testHeapSetGet = TestCase (assertBool "heap set get" testHeapSetGetImpl)

testHeapSetGetImpl :: Bool
testHeapSetGetImpl =
  value == 42
  where
    value = Data.fromMaybe 98764321 (heapGet (heapSet ctx 1 42) 1)
    ctx = case heapAlloc (Just newContext) of
      Nothing -> Nothing
      Just (_, Just two) -> Just two
      Just (_, Nothing) -> Nothing

testLabelSetGetImpl :: Bool
testLabelSetGetImpl =
  value == Just 42
  where
    value = labelGet c "ouioui"
    c = labelSet (Just newContext) "ouioui" 42

testLabelSetGet :: Test
testLabelSetGet = TestCase (assertBool "label set get" testLabelSetGetImpl)

testFlagGetSetImpl :: Bool
testFlagGetSetImpl =
  value == True
  where
    value = flagGet c ZF
    c = flagSet (Just newContext) ZF True

testFlagGetSet :: Test
testFlagGetSet = TestCase (assertBool "flag get set" testFlagGetSetImpl)

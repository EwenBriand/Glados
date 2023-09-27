module Instructions
  ( myCmp,
    myTest,
    myJmp,
    myJe,
    myJne,
    myJs,
    myJns,
    myJg,
    myJge,
    myJl,
    myJle,
    myJa,
    myJae,
    myJb,
    myJbe,
    myInc,
    myDec,
    myNeg,
    instructionTable,
    allTest,
  )
where

-- labelAlloc,

import Data.Bits
import Data.Maybe
import VM
  ( Context (..),
    Flag (..),
    Flags (..),
    Heap (..),
    Instruction (..),
    Labels (..),
    Param (..),
    Register (..),
    Registers (..),
    Stack (..),
    SymTable (..),
    flagGet,
    flagSet,
    getTrueValueFromParam,
    heapAlloc,
    heapFree,
    heapGet,
    heapSet,
    ipGet,
    ipInc,
    ipSet,
    labelFree,
    labelGet,
    labelSet,
    newContext,
    newFlags,
    newHeap,
    newLabels,
    newRegisters,
    newStack,
    newSymTable,
    regAdd,
    regAnd,
    regDec,
    regDiv,
    regGet,
    regInc,
    regMod,
    regMul,
    regOr,
    regSet,
    regSub,
    regXor,
    setTrueValueFromParam,
    stackDup,
    stackPeek,
    stackPop,
    stackPush,
    stackRot,
    stackSwap,
    symAlloc,
    symFree,
    symGet,
    symSet,
  )

instructionTable :: Maybe Context -> Instruction -> Maybe Context
instructionTable Nothing _ = Nothing
instructionTable ctx (Mov r1 r2) = allMov ctx r1 r2
instructionTable ctx (Cmp r1 r2) = allCmp ctx r1 r2
instructionTable ctx (Test r1 r2) = allTest ctx r1 r2
instructionTable ctx (Jmp r1) = myJmp ctx r1
instructionTable ctx (Je r1) = myJe ctx r1
instructionTable ctx (Jne r1) = myJne ctx r1
instructionTable ctx (Js r1) = myJs ctx r1
instructionTable ctx (Jns r1) = myJns ctx r1
instructionTable ctx (Jg r1) = myJg ctx r1
instructionTable ctx (Jge r1) = myJge ctx r1
instructionTable ctx (Jl r1) = myJl ctx r1
instructionTable ctx (Jle r1) = myJle ctx r1
instructionTable ctx (Ja r1) = myJa ctx r1
instructionTable ctx (Jae r1) = myJae ctx r1
instructionTable ctx (Jb r1) = myJb ctx r1
instructionTable ctx (Jbe r1) = myJbe ctx r1
instructionTable ctx (Inc r1) = myInc ctx r1 (regGet ctx r1)
instructionTable ctx (Dec r1) = myDec ctx r1 (regGet ctx r1)
instructionTable ctx (Neg r1) = myNeg ctx r1 (regGet ctx r1)
instructionTable ctx (Add r1 r2) = allAdd ctx r1 r2
instructionTable ctx (Push r1) = pushImpl ctx r1
instructionTable ctx (Xor r1 r2) = xorImpl ctx r1 r2
instructionTable _ _ = Nothing

--
-- PUSH SECTION
--

pushImpl :: Maybe Context -> Param -> Maybe Context
pushImpl Nothing _ = Nothing
pushImpl ctx ri = case getTrueValueFromParam ctx ri of
  Nothing -> Nothing
  Just val -> stackPush ctx val

--
-- POP SECTION
--

popImpl :: Maybe Context -> Param -> Maybe Context
popImpl Nothing _ = Nothing
popImpl _ (Immediate _) = Nothing
popImpl ctx param = case stackPop ctx of
  Just (val, c) -> setTrueValueFromParam c param val
  _ -> Nothing

--
-- MOVE SECTION
--

movImpl :: Maybe Context -> Param -> Param -> Maybe Context
movImpl Nothing _ _ = Nothing
movImpl _ (Immediate _) _ = Nothing
movImpl ctx to from = case getTrueValueFromParam ctx from of
  Just val -> setTrueValueFromParam ctx to val
  _ -> Nothing

allMov :: Maybe Context -> Register -> Param -> Maybe Context
allMov Nothing _ _ = Nothing
allMov ctx r1 (Reg r2) = myMovReg ctx r1 (regGet ctx r2)
allMov ctx r1 (Immediate r2) = myMovInt ctx r1 r2
allMov ctx r1 (Memory r2) = myMovMem ctx r1 r2
allMov ctx r1 (Symbol r2) = myMovSym ctx r1 r2
allMov _ _ _ = Nothing

-- | @params: register
myMovReg :: Maybe Context -> Register -> Maybe Int -> Maybe Context
myMovReg Nothing _ _ = Nothing
myMovReg _ _ Nothing = Nothing
myMovReg ctx r1 (Just r2) = regSet ctx r1 r2

-- | @params: register, immediate
myMovInt :: Maybe Context -> Register -> Int -> Maybe Context
myMovInt Nothing _ _ = Nothing
myMovInt ctx r1 r2 = regSet ctx r1 r2

-- | @params: register, memory
myMovMem :: Maybe Context -> Register -> Int -> Maybe Context
myMovMem Nothing _ _ = Nothing
myMovMem ctx r1 r2 = case heapGet ctx r2 of
  Nothing -> Nothing
  Just val -> regSet ctx r1 val

-- | @params: register, symbol
myMovSym :: Maybe Context -> Register -> String -> Maybe Context
myMovSym Nothing _ _ = Nothing
myMovSym ctx r1 r2 = case symGet ctx r2 of
  Nothing -> Nothing
  Just val -> regSet ctx r1 val

--
-- Comp SECTION
--

allCmp :: Maybe Context -> Param -> Param -> Maybe Context
allCmp Nothing _ _ = Nothing
allCmp ctx (Reg r1) (Reg r2) = myCmp ctx (regGet ctx r1) (regGet ctx r2)
allCmp ctx (Reg r1) (Immediate r2) = myCmp ctx (regGet ctx r1) (Just r2)
allCmp ctx (Reg r1) (Memory r2) = myCmp ctx (regGet ctx r1) (heapGet ctx r2)
allCmp ctx (Reg r1) (Symbol r2) = myCmp ctx (regGet ctx r1) (symGet ctx r2)
allCmp _ _ _ = Nothing

myCmp :: Maybe Context -> Maybe Int -> Maybe Int -> Maybe Context
myCmp Nothing _ _ = Nothing
myCmp _ Nothing _ = Nothing
myCmp _ _ Nothing = Nothing
myCmp ctx (Just val1) (Just val2) = do
  let res = val1 - val2
  c1 <- flagSet ctx ZF (res == 0)
  c2 <- flagSet (Just c1) SF (res < 0)
  c3 <- flagSet (Just c2) OF (val1 < 0 && val2 > 0 && res > 0 || val1 > 0 && val2 < 0 && res < 0)
  c4 <- flagSet (Just c3) CF (val1 < val2)
  Just c4

allTest :: Maybe Context -> Param -> Param -> Maybe Context
allTest Nothing _ _ = Nothing
allTest ctx (Reg r1) (Reg r2) = myTest ctx (regGet ctx r1) (regGet ctx r2)
allTest ctx (Reg r1) (Immediate r2) = myTest ctx (regGet ctx r1) (Just r2)
allTest ctx (Reg r1) (Memory r2) = myTest ctx (regGet ctx r1) (heapGet ctx r2)
allTest ctx (Reg r1) (Symbol r2) = myTest ctx (regGet ctx r1) (symGet ctx r2)
allTest _ _ _ = Nothing

myTest :: Maybe Context -> Maybe Int -> Maybe Int -> Maybe Context
myTest Nothing _ _ = Nothing
myTest _ Nothing _ = Nothing
myTest ctx (Just val1) (Just val2) = do
  let res = val1 .&. val2
  c1 <- flagSet ctx ZF (res == 0)
  c2 <- flagSet (Just c1) SF (res < 0)
  c3 <- flagSet (Just c2) OF False
  c4 <- flagSet (Just c3) CF False
  Just c4
myTest _ _ _ = Nothing

--
-- JUMP SECTION
--

myJmp :: Maybe Context -> String -> Maybe Context
myJmp Nothing _ = Nothing
myJmp ctx label = case labelGet ctx label of
  Nothing -> Nothing
  Just val -> ipSet ctx (val - 1)

myJe :: Maybe Context -> String -> Maybe Context
myJe Nothing _ = Nothing
myJe ctx label = if flagGet ctx ZF then myJmp ctx label else ctx

myJne :: Maybe Context -> String -> Maybe Context
myJne Nothing _ = Nothing
myJne ctx label = if not (flagGet ctx ZF) then myJmp ctx label else ctx

myJs :: Maybe Context -> String -> Maybe Context
myJs Nothing _ = Nothing
myJs ctx label = if flagGet ctx SF then myJmp ctx label else ctx

myJns :: Maybe Context -> String -> Maybe Context
myJns Nothing _ = Nothing
myJns ctx label = if not (flagGet ctx SF) then myJmp ctx label else ctx

myJg :: Maybe Context -> String -> Maybe Context
myJg Nothing _ = Nothing
myJg ctx label = if not (flagGet ctx ZF) && flagGet ctx SF == flagGet ctx OF then myJmp ctx label else ctx

myJge :: Maybe Context -> String -> Maybe Context
myJge Nothing _ = Nothing
myJge ctx label = if flagGet ctx SF == flagGet ctx OF then myJmp ctx label else ctx

myJl :: Maybe Context -> String -> Maybe Context
myJl Nothing _ = Nothing
myJl ctx label = if not (flagGet ctx ZF) && flagGet ctx SF /= flagGet ctx OF then myJmp ctx label else ctx

myJle :: Maybe Context -> String -> Maybe Context
myJle Nothing _ = Nothing
myJle ctx label = if flagGet ctx ZF || flagGet ctx SF /= flagGet ctx OF then myJmp ctx label else ctx

myJa :: Maybe Context -> String -> Maybe Context
myJa Nothing _ = Nothing
myJa ctx label = if not (flagGet ctx CF) && flagGet ctx ZF == False then myJmp ctx label else ctx

myJae :: Maybe Context -> String -> Maybe Context
myJae Nothing _ = Nothing
myJae ctx label = if not (flagGet ctx CF) then myJmp ctx label else ctx

myJb :: Maybe Context -> String -> Maybe Context
myJb Nothing _ = Nothing
myJb ctx label = if flagGet ctx CF then myJmp ctx label else ctx

myJbe :: Maybe Context -> String -> Maybe Context
myJbe Nothing _ = Nothing
myJbe ctx label = if flagGet ctx CF || flagGet ctx ZF /= False then myJmp ctx label else ctx

--
-- Inc SECTION
--

myInc :: Maybe Context -> Register -> Maybe Int -> Maybe Context
myInc Nothing _ _ = Nothing
myInc _ _ Nothing = Nothing
myInc ctx r (Just r1) = regSet ctx r (r1 + 1)

myDec :: Maybe Context -> Register -> Maybe Int -> Maybe Context
myDec Nothing _ _ = Nothing
myDec _ _ Nothing = Nothing
myDec ctx r (Just r1) = regSet ctx r (r1 - 1)

myNeg :: Maybe Context -> Register -> Maybe Int -> Maybe Context
myNeg Nothing _ _ = Nothing
myNeg _ _ Nothing = Nothing
myNeg ctx r (Just r1) = regSet ctx r (- r1)

allAdd :: Maybe Context -> Register -> Param -> Maybe Context
allAdd Nothing _ _ = Nothing
allAdd ctx r1 (Reg r2) = myAdd ctx r1 (regGet ctx r1) (regGet ctx r2)
allAdd ctx r1 (Immediate r2) = myAdd ctx r1 (regGet ctx r1) (Just r2)
allAdd ctx r1 (Memory r2) = case heapGet ctx r2 of
  Nothing -> Nothing
  Just val -> myAdd ctx r1 (regGet ctx r1) (Just val)
allAdd ctx r1 (Symbol r2) = case symGet ctx r2 of
  Nothing -> Nothing
  Just val -> myAdd ctx r1 (regGet ctx r1) (Just val)

myAdd :: Maybe Context -> Register -> Maybe Int -> Maybe Int -> Maybe Context
myAdd Nothing _ _ _ = Nothing
myAdd _ _ Nothing _ = Nothing
myAdd _ _ _ Nothing = Nothing
myAdd ctx r (Just r1) (Just r2) = regSet ctx r (r1 + r2)

--
-- XOR SECTION
--

xorImpl :: Maybe Context -> Param -> Param -> Maybe Context
xorImpl _ (Immediate _) _ = Nothing
xorImpl ctx p1 p2 = c
  where
    c = setTrueValueFromParam ctx p1 xoredVal
    xoredVal = fromMaybe 0 (xor <$> getTrueValueFromParam ctx p1 <*> getTrueValueFromParam ctx p2)

--
-- Enter SECTION
--

-- | The enter instruction is equivalent to the following pseudo-code:
-- push ebp
-- mov ebp, esp
enterImpl :: Context -> Maybe Context
enterImpl ctx = movImpl c (Reg EBP) (Reg ESP)
  where
    c = pushImpl (Just ctx) (Reg EBP)

--
-- Leave SECTION
--

-- | The leave instruction is equivalent to the following pseudo-code:
-- mov esp, ebp
-- pop ebp
leaveImpl :: Maybe Context -> Maybe Context
leaveImpl ctx = ctx1
  where
    ctx1 = popImpl c (Reg EBP)
    c = movImpl ctx (Reg ESP) (Reg EBP)

--
-- Add SECTION
--

-- | When the Add instrcution is called, it updates the flags as follows:
-- updates the sign flag (SF) to the most significant bit of the result
-- updates the zero flag (ZF) if the result is zero
-- updates the overflow flag (OF) if the result is too large a positive number or too small a negative number (excluding the sign-bit) to fit in the destination operand

-- updateFlagsAdd :: Maybe Context -> Param -> Param -> Maybe Int -> Maybe Context
-- updateFlagAdd _ _ _ Nothing = Nothing
-- updateFlagsAdd ctx p1 p2 addedVal = ctx4
--     where
--         ctx4 = flagSet ctx3 CF (addedVal < (getTrueValueFromParam ctx1 p1))
--         ctx3 = flagSet ctx2 OF (addedVal < (getTrueValueFromParam ctx1 p1))
--         ctx2 = flagSet ctx1 SF (addedVal < 0)
--         ctx1 = flagSet ctx ZF (addedVal == 0)

-- addImpl :: Maybe Context -> Param -> Param -> Maybe Context
-- addImpl _ (Immediate _) _ = Nothing
-- addImpl ctx1 p1 p2 = c
--     where
--         c = updateFlagsAdd ctx1 p1 p2 (Just addedVal)
--         tmpCtx = case addedVal of
--           Nothing -> Nothing
--           Just v -> setTrueValueFromParam ctx1 p1 v
--         addedVal = fromMaybe (getTrueValueFromParam ctx1 p1) + fromMaybe (getTrueValueFromParam ctx1 p2)

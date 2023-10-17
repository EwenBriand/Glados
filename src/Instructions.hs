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
    execInstructions,
    nbInstructions,
    evalOneInstruction,
    movStackAddrImpl,
    evalOneInstructionIO,
    execInstructionsIO,
    movFromStackAddrImpl,
    blkSetupCtx,
    setupfunctionStack,
    instructionTableIO,
    allocHeap,
  )
where

-- labelAlloc,

-- labelAlloc,

import Data.Bits
import Lexer (VarType)
import VM
import ValidState

instructionTable :: ValidState Context -> Instruction -> ValidState Context
instructionTable (Invalid s) _ = Invalid s
instructionTable ctx instr = fst (instructionTableIO ctx instr)

allocHeap :: ValidState Context -> Int -> ValidState Context
allocHeap (Invalid s) _ = Invalid s
allocHeap ctx int = regSet context EAX addr
  where
    (addr, context) = heapAllocRange ctx int

setupfunctionStack :: ValidState Context -> ValidState Context -> [VarType] -> [Register] -> ValidState Context
setupfunctionStack (Invalid s) _ _ _ = Invalid s
setupfunctionStack (Valid _) (Invalid s) _ _ = Invalid s
setupfunctionStack (Valid _) (Valid ctx') [] _ = Valid ctx'
setupfunctionStack (Valid ctx) (Valid ctx') (_ : ts) (r : rs) = setupfunctionStack (Valid ctx) ctx'' ts rs
  where
    ctx'' = stackPush (Valid ctx') (fromValidState 0 (getTrueValueFromParam (Valid ctx) (Reg r)))
setupfunctionStack _ _ _ _ = Invalid "Invalid function call"

blkSetupCtx :: Context -> Block -> Block
blkSetupCtx ctx (Block name bc paramsTypes) = Block name c' paramsTypes
  where
    c' = case c'' of
      Invalid s -> Invalid s
      Valid c -> Valid (c {blocks = blocks ctx, instructionPointer = 0})
    c'' = setupfunctionStack (Valid ctx) (stackClear bc) paramsTypes [EDI, ESI, EDX, ECX]

-- c' = Block name (execInstructions (detectLabels (setupFunctionStack bc ctx))) paramsTypes
---
--- IO Logic
---

instructionTableIO :: ValidState Context -> Instruction -> (ValidState Context, IO ())
-- instructionTableIO (Invalid s) _ = (Invalid s, putStrLn s)
instructionTableIO ctx Interrupt = execSyscallWrapper ctx
instructionTableIO ctx (Mov r1 r2) = (movImpl ctx r1 r2, putStr "")
instructionTableIO ctx (Cmp r1 r2) = (allCmp ctx r1 r2, putStr "")
instructionTableIO ctx (Test r1 r2) = (allTest ctx r1 r2, putStr "")
instructionTableIO ctx (Jmp r1) = (myJmp ctx r1, putStr "")
instructionTableIO ctx (Je r1) = (myJe ctx r1, putStr "")
instructionTableIO ctx (Jne r1) = (myJne ctx r1, putStr "")
instructionTableIO ctx (Js r1) = (myJs ctx r1, putStr "")
instructionTableIO ctx (Jns r1) = (myJns ctx r1, putStr "")
instructionTableIO ctx (Jg r1) = (myJg ctx r1, putStr "")
instructionTableIO ctx (Jge r1) = (myJge ctx r1, putStr "")
instructionTableIO ctx (Jl r1) = (myJl ctx r1, putStr "")
instructionTableIO ctx (Jle r1) = (myJle ctx r1, putStr "")
instructionTableIO ctx (Ja r1) = (myJa ctx r1, putStr "")
instructionTableIO ctx (Jae r1) = (myJae ctx r1, putStr "")
instructionTableIO ctx (Jb r1) = (myJb ctx r1, putStr "")
instructionTableIO ctx (Jbe r1) = (myJbe ctx r1, putStr "")
instructionTableIO ctx (Inc r1) = (myInc ctx r1 (regGet ctx r1), putStr "")
instructionTableIO ctx (Dec r1) = (myDec ctx r1 (regGet ctx r1), putStr "")
instructionTableIO ctx (Neg r1) = (myNeg ctx r1 (regGet ctx r1), putStr "")
instructionTableIO ctx (Add r1 r2) = (allAdd ctx r1 r2, putStr "")
instructionTableIO ctx (Sub r1 r2) = (subImpl ctx r1 r2, putStr "")
instructionTableIO ctx (Mult r1 r2) = (multImpl ctx r1 r2, putStr "")
instructionTableIO ctx (Div r1) = (divImpl ctx r1, putStr "")
instructionTableIO ctx (Push r1) = (pushImpl ctx r1, putStr "")
instructionTableIO ctx (Pop r1) = (popImpl ctx r1, putStr "")
instructionTableIO ctx (Xor r1 r2) = (xorImpl ctx r1 r2, putStr "")
instructionTableIO ctx (And r1 r2) = (andImpl ctx r1 r2, putStr "")
instructionTableIO ctx (Or r1 r2) = (orImpl ctx r1 r2, putStr "")
instructionTableIO ctx (Not r1) = (notImpl ctx r1, putStr "")
instructionTableIO ctx (MovPtr p1 p2) = (movPtrImpl ctx p1 p2, putStr "")
instructionTableIO ctx Nop = (ctx, putStr "")
instructionTableIO ctx (IMul _ _) = (ctx, putStr "")
instructionTableIO ctx Enter = (enterImpl (fromValidState newContext ctx), putStr "")
instructionTableIO ctx Leave = (leaveImpl ctx, putStr "")
instructionTableIO ctx (Label _ _) = (ctx, putStr "") -- labels are preprocessed before executing
instructionTableIO ctx (MovStackAddr p1 p2) = (movStackAddrImpl ctx p1 p2, putStr "")
instructionTableIO ctx (MovFromStackAddr p1 p2) = (movFromStackAddrImpl ctx p1 p2, putStr "")
instructionTableIO ctx (Call str) = callImpl ctx str
instructionTableIO ctx (Alloc int) = (allocHeap ctx int, putStr "")

evalOneInstructionIO :: Context -> Instruction -> (ValidState Context, IO ())
evalOneInstructionIO ctx = instructionTableIO (Valid ctx)

execInstructionsIO :: (ValidState Context, IO ()) -> (ValidState Context, IO ())
execInstructionsIO (context, prevIO) =
  case c of
    (Invalid s, io) -> (Invalid s, prevIO >> io)
    (ct, io) -> if fromValidState (-1) (ipGet ct) + 1 > nbInstructions ct then (ct, prevIO >> io) else execInstructionsIO (ipInc ct, prevIO >> io)
  where
    c =
      if fromValidState (-1) (ipGet context) + 1 > nbInstructions context
        then (context, prevIO)
        else evalOneInstructionIO (fromValidState newContext context) (getInsIndex context (fromValidState (-1) (ipGet context)))

---
--- Context Logic
---

executeBlock :: ValidState Context -> Block -> (ValidState Context, IO ())
executeBlock (Invalid s) _ = (Invalid s, putStr "")
executeBlock (Valid c) block = do
  let b = blkSetupCtx c block
  case execInstructionsIO (detectLabels (blockContext b), putStr "") of
    (Invalid s, _) -> (Invalid ("While executing block " ++ blockName b ++ ": " ++ s), putStr "")
    (Valid executed, io) -> case getTrueValueFromParam (Valid executed) (Reg EAX) of
      Invalid s -> (Invalid ("While executing block " ++ blockName b ++ ": " ++ s), putStr "invalid in block")
      Valid v -> (regSet (Valid c) EAX v, io)

callImpl :: ValidState Context -> String -> (ValidState Context, IO ())
callImpl (Invalid s) _ = (Invalid s, putStr "")
callImpl (Valid c) symName = case blockGet (Valid c) symName of
  Invalid s -> (Invalid s, putStr "")
  Valid block -> executeBlock (Valid c) block

-- | Evaluates one instruction and Prelude.returns the resulting context. Does not increase the instruction count.
evalOneInstruction :: Context -> Instruction -> ValidState Context
evalOneInstruction ctx = instructionTable (Valid ctx)

-- | Executes all the instructions until the instruction pointer reaches the end of the program.
-- Increases the instruction pointer after each call.
execInstructions :: ValidState Context -> ValidState Context
execInstructions context =
  case c of
    Invalid s -> Invalid s
    ct -> if fromValidState (-1) (ipGet ct) + 1 > nbInstructions ct then ct else execInstructions (ipInc ct)
  where
    c =
      if fromValidState (-1) (ipGet context) + 1 > nbInstructions context
        then context
        else evalOneInstruction (fromValidState newContext context) (getInsIndex context (fromValidState (-1) (ipGet context)))

getInsIndex :: ValidState Context -> Int -> Instruction
getInsIndex (Invalid _) _ = Nop
getInsIndex (Valid context) i = if i < length (instructions context) then instructions context !! i else Nop

nbInstructions :: ValidState Context -> Int
nbInstructions (Invalid _) = -1
nbInstructions (Valid context) = length (instructions context)

--
-- PUSH SECTION
--

pushImpl :: ValidState Context -> Param -> ValidState Context
pushImpl (Invalid s) _ = Invalid s
pushImpl ctx ri = case getTrueValueFromParam ctx ri of
  Invalid s -> Invalid s
  Valid val -> stackPush ctx val

--
-- POP SECTION
--

popImpl :: ValidState Context -> Param -> ValidState Context
popImpl (Invalid s) _ = Invalid s
popImpl _ (Immediate _) = Invalid "Cannot pop into an immediate"
popImpl ctx param = case stackPop ctx of
  Valid (val, c) -> setTrueValueFromParam c param val
  _ -> Invalid "Stack pop failed"

--
-- MOVE SECTION
--

movPtrImpl :: ValidState Context -> Param -> Param -> ValidState Context
movPtrImpl (Invalid s) p _ = Invalid ("While assigning to pointer " ++ show p ++ ": " ++ s)
movPtrImpl _ (Immediate _) _ = Invalid "Cannot move into an immediate"
movPtrImpl ctx (Reg r) p = case getTrueValueFromParam ctx p of
  Invalid s -> Invalid s
  Valid val -> case getTrueValueFromParam ctx (Reg r) of
    Invalid s -> Invalid s
    Valid ptr -> heapSet ctx ptr val
movPtrImpl ctx (Memory r) p = case getTrueValueFromParam ctx p of
  Invalid s -> Invalid s
  Valid val -> case getTrueValueFromParam ctx (Memory r) of
    Invalid s -> Invalid s
    Valid ptr -> heapSet ctx ptr val
movPtrImpl _ _ _ = Invalid "Invalid move"

setArrayIndex :: Int -> Int -> [Int] -> [Int]
setArrayIndex _ _ [] = []
setArrayIndex 0 val (_ : xs) = val : xs
setArrayIndex idx val (x : xs) = x : setArrayIndex (idx - 1) val xs

setStackIndex :: ValidState Context -> Int -> Int -> ValidState Context
setStackIndex (Invalid s) _ _ = Invalid s
setStackIndex (Valid ctx) idx value =
  Valid
    ctx
      { stack = Stack (setArrayIndex idx value (pile (stack ctx)))
      }

-- sets the value at ebp + address
movStackAddrImpl :: ValidState Context -> Param -> Param -> ValidState Context
movStackAddrImpl (Invalid s) _ _ = Invalid s
movStackAddrImpl ctx to from = case getTrueValueFromParam ctx from of
  Invalid s -> Invalid s
  Valid val -> case regGet ctx ESP of
    Invalid s -> Invalid s
    Valid _ -> case getTrueValueFromParam ctx to of
      Invalid s -> Invalid s
      Valid addr -> setStackIndex ctx addr val

movFromStackAddrImpl :: ValidState Context -> Param -> Param -> ValidState Context
movFromStackAddrImpl (Invalid s) _ _ = Invalid s
movFromStackAddrImpl (Valid ctx) dest addr = case getTrueValueFromParam (Valid ctx) addr of
  Invalid s -> Invalid s
  Valid addr' -> setTrueValueFromParam (Valid ctx) dest (pile (stack ctx) !! addr')

movImpl :: ValidState Context -> Param -> Param -> ValidState Context
movImpl (Invalid s) _ _ = Invalid s
movImpl _ (Immediate _) _ = Invalid "Cannot move into an immediate"
movImpl ctx to from = case getTrueValueFromParam ctx from of
  Valid val -> setTrueValueFromParam ctx to val
  _ -> Invalid "Invalid move"

--
-- Comp SECTION
--

allCmp :: ValidState Context -> Param -> Param -> ValidState Context
allCmp (Invalid s) _ _ = Invalid ("During comparison: " ++ s)
allCmp ctx (Reg r1) (Reg r2) = myCmp ctx (regGet ctx r1) (regGet ctx r2)
allCmp ctx (Reg r1) (Immediate r2) = myCmp ctx (regGet ctx r1) (Valid r2)
allCmp ctx (Reg r1) (Memory r2) = myCmp ctx (regGet ctx r1) (heapGet ctx r2)
allCmp ctx (Reg r1) (Symbol r2) = myCmp ctx (regGet ctx r1) (symGet ctx r2)
allCmp _ _ _ = Invalid "Invalid cmp"

myCmp :: ValidState Context -> ValidState Int -> ValidState Int -> ValidState Context
myCmp (Invalid s) _ _ = Invalid s
myCmp _ (Invalid s) _ = Invalid s
myCmp _ _ (Invalid s) = Invalid s
myCmp ctx (Valid val1) (Valid val2) = do
  let res = val1 - val2
  c1 <- flagSet ctx ZF (res == 0)
  c2 <- flagSet (Valid c1) SF (res < 0)
  c3 <- flagSet (Valid c2) OF (val1 < 0 && val2 > 0 && res > 0 || val1 > 0 && val2 < 0 && res < 0)
  c4 <- flagSet (Valid c3) CF (val1 < val2)
  Valid c4

allTest :: ValidState Context -> Param -> Param -> ValidState Context
allTest (Invalid s) _ _ = Invalid s
allTest ctx (Reg r1) (Reg r2) = myTest ctx (regGet ctx r1) (regGet ctx r2)
allTest ctx (Reg r1) (Immediate r2) = myTest ctx (regGet ctx r1) (Valid r2)
allTest ctx (Reg r1) (Memory r2) = myTest ctx (regGet ctx r1) (heapGet ctx r2)
allTest ctx (Reg r1) (Symbol r2) = myTest ctx (regGet ctx r1) (symGet ctx r2)
allTest _ _ _ = Invalid "Invalid test"

myTest :: ValidState Context -> ValidState Int -> ValidState Int -> ValidState Context
myTest (Invalid s) _ _ = Invalid s
myTest _ (Invalid s) _ = Invalid s
myTest ctx (Valid val1) (Valid val2) = do
  let res = val1 .&. val2
  c1 <- flagSet ctx ZF (res == 0)
  c2 <- flagSet (Valid c1) SF (res < 0)
  c3 <- flagSet (Valid c2) OF False
  c4 <- flagSet (Valid c3) CF False
  Valid c4
myTest _ _ _ = Invalid "Invalid test"

--
-- JUMP SECTION
--

myJmp :: ValidState Context -> String -> ValidState Context
myJmp (Invalid s) _ = Invalid s
myJmp ctx label = case labelGet ctx label of
  Invalid s -> Invalid s
  Valid val -> ipSet ctx (val - 1)

myJe :: ValidState Context -> String -> ValidState Context
myJe (Invalid s) _ = Invalid s
myJe ctx label = if flagGet ctx ZF then myJmp ctx label else ctx

myJne :: ValidState Context -> String -> ValidState Context
myJne (Invalid s) _ = Invalid s
myJne ctx label = if not (flagGet ctx ZF) then myJmp ctx label else ctx

myJs :: ValidState Context -> String -> ValidState Context
myJs (Invalid s) _ = Invalid s
myJs ctx label = if flagGet ctx SF then myJmp ctx label else ctx

myJns :: ValidState Context -> String -> ValidState Context
myJns (Invalid s) _ = Invalid s
myJns ctx label = if not (flagGet ctx SF) then myJmp ctx label else ctx

myJg :: ValidState Context -> String -> ValidState Context
myJg (Invalid s) _ = Invalid s
myJg ctx label = if not (flagGet ctx ZF) && flagGet ctx SF == flagGet ctx OF then myJmp ctx label else ctx

myJge :: ValidState Context -> String -> ValidState Context
myJge (Invalid s) _ = Invalid s
myJge ctx label = if flagGet ctx SF == flagGet ctx OF then myJmp ctx label else ctx

myJl :: ValidState Context -> String -> ValidState Context
myJl (Invalid s) _ = Invalid s
myJl ctx label = if not (flagGet ctx ZF) && flagGet ctx SF /= flagGet ctx OF then myJmp ctx label else ctx

myJle :: ValidState Context -> String -> ValidState Context
myJle (Invalid s) _ = Invalid s
myJle ctx label = if flagGet ctx ZF || flagGet ctx SF /= flagGet ctx OF then myJmp ctx label else ctx

myJa :: ValidState Context -> String -> ValidState Context
myJa (Invalid s) _ = Invalid s
myJa ctx label = if not (flagGet ctx CF) && flagGet ctx ZF == False then myJmp ctx label else ctx

myJae :: ValidState Context -> String -> ValidState Context
myJae (Invalid s) _ = Invalid s
myJae ctx label = if not (flagGet ctx CF) then myJmp ctx label else ctx

myJb :: ValidState Context -> String -> ValidState Context
myJb (Invalid s) _ = Invalid s
myJb ctx label = if flagGet ctx CF then myJmp ctx label else ctx

myJbe :: ValidState Context -> String -> ValidState Context
myJbe (Invalid s) _ = Invalid s
myJbe ctx label = if flagGet ctx CF || flagGet ctx ZF /= False then myJmp ctx label else ctx

--
-- Inc SECTION
--

myInc :: ValidState Context -> Register -> ValidState Int -> ValidState Context
myInc (Invalid s) _ _ = Invalid s
myInc _ _ (Invalid s) = Invalid s
myInc ctx r (Valid r1) = regSet ctx r (r1 + 1)

myDec :: ValidState Context -> Register -> ValidState Int -> ValidState Context
myDec (Invalid s) _ _ = Invalid s
myDec _ _ (Invalid s) = Invalid s
myDec ctx r (Valid r1) = regSet ctx r (r1 - 1)

myNeg :: ValidState Context -> Register -> ValidState Int -> ValidState Context
myNeg (Invalid s) _ _ = Invalid s
myNeg _ _ (Invalid s) = Invalid s
myNeg ctx r (Valid r1) = regSet ctx r (- r1)

allAdd :: ValidState Context -> Register -> Param -> ValidState Context
allAdd (Invalid s) _ _ = Invalid s
allAdd ctx r1 (Reg r2) = myAdd ctx r1 (regGet ctx r1) (regGet ctx r2)
allAdd ctx r1 (Immediate r2) = myAdd ctx r1 (regGet ctx r1) (Valid r2)
allAdd ctx r1 (Memory r2) = case heapGet ctx r2 of
  Invalid s -> Invalid ("During addition from pointer: " ++ s)
  Valid val -> myAdd ctx r1 (regGet ctx r1) (Valid val)
allAdd ctx r1 (Symbol r2) = case symGet ctx r2 of
  Invalid s -> Invalid s
  Valid val -> myAdd ctx r1 (regGet ctx r1) (Valid val)

myAdd :: ValidState Context -> Register -> ValidState Int -> ValidState Int -> ValidState Context
myAdd (Invalid s) _ _ _ = Invalid s
myAdd _ _ (Invalid s) _ = Invalid s
myAdd _ _ _ (Invalid s) = Invalid s
myAdd ctx r (Valid r1) (Valid r2) = regSet ctx r (r1 + r2)

--
-- XOR SECTION
--

xorImpl :: ValidState Context -> Param -> Param -> ValidState Context
xorImpl _ (Immediate _) _ = Invalid "Cannot xor into an immediate"
xorImpl (Invalid s) _ _ = Invalid s
xorImpl ctx p1 p2 = c
  where
    c = setTrueValueFromParam ctx p1 xoredVal
    xoredVal = fromValidState 0 (xor <$> getTrueValueFromParam ctx p1 <*> getTrueValueFromParam ctx p2)

divImpl :: ValidState Context -> Param -> ValidState Context
divImpl (Invalid s) _ = Invalid s
divImpl ctx p2 = c
  where
    c = setTrueValueFromParam c1 (Reg EDX) modVal
    c1 = setTrueValueFromParam ctx (Reg EAX) divVal
    divVal = fromValidState 0 (div <$> getTrueValueFromParam ctx (Reg EAX) <*> getTrueValueFromParam ctx p2)
    modVal = fromValidState 0 (getTrueValueFromParam ctx (Reg EAX)) `mod` fromValidState 0 (getTrueValueFromParam ctx p2)

multImpl :: ValidState Context -> Param -> Param -> ValidState Context
multImpl _ (Immediate _) _ = Invalid "Cannot mult into an immediate"
multImpl (Invalid s) _ _ = Invalid s
multImpl ctx p1 p2 = c
  where
    c = setTrueValueFromParam ctx p1 multVal
    multVal = fromValidState 0 (getTrueValueFromParam ctx p1) * fromValidState 0 (getTrueValueFromParam ctx p2)

subImpl :: ValidState Context -> Param -> Param -> ValidState Context
subImpl _ (Immediate _) _ = Invalid "Cannot sub into an immediate"
subImpl (Invalid s) _ _ = Invalid s
subImpl ctx p1 p2 = c
  where
    c = setTrueValueFromParam ctx p1 multVal
    multVal = fromValidState 0 (getTrueValueFromParam ctx p1) - fromValidState 0 (getTrueValueFromParam ctx p2)

andImpl :: ValidState Context -> Param -> Param -> ValidState Context
andImpl _ (Immediate _) _ = Invalid "Cannot and into an immediate"
andImpl (Invalid s) _ _ = Invalid s
andImpl ctx p1 p2 = c
  where
    c = setTrueValueFromParam ctx p1 modVal
    modVal = fromValidState 0 (getTrueValueFromParam ctx p1) .&. fromValidState 0 (getTrueValueFromParam ctx p2)

orImpl :: ValidState Context -> Param -> Param -> ValidState Context
orImpl _ (Immediate _) _ = Invalid "Cannot or into an immediate"
orImpl (Invalid s) _ _ = Invalid s
orImpl ctx p1 p2 = c
  where
    c = setTrueValueFromParam ctx p1 modVal
    modVal = fromValidState 0 (getTrueValueFromParam ctx p1) .|. fromValidState 0 (getTrueValueFromParam ctx p2)

notImpl :: ValidState Context -> Param -> ValidState Context
notImpl _ (Immediate _) = Invalid "Cannot not into an immediate"
notImpl (Invalid s) _ = Invalid s
notImpl ctx p1 = c
  where
    c = setTrueValueFromParam ctx p1 modVal
    modVal = complement (fromValidState 0 (getTrueValueFromParam ctx p1)) .&. 0xFF

--
-- Enter SECTION
--

-- | The enter instruction is equivalent to the following pseudo-code:
-- push ebp
-- mov ebp, esp
enterImpl :: Context -> ValidState Context
enterImpl ctx = movImpl c (Reg EBP) (Reg ESP)
  where
    c = pushImpl (Valid ctx) (Reg EBP)

--
-- Leave SECTION
--

-- | The leave instruction is equivalent to the following pseudo-code:
-- mov esp, ebp
-- pop ebp
leaveImpl :: ValidState Context -> ValidState Context
leaveImpl ctx = ctx1
  where
    ctx1 = popImpl c (Reg EBP)
    c = movImpl ctx (Reg ESP) (Reg EBP)

{-# LANGUAGE DeriveGeneric #-}

module VM
  ( Register (..),
    Registers (..),
    newRegisters,
    regSet,
    regGet,
    regInc,
    regDec,
    regAdd,
    regSub,
    regMul,
    regDiv,
    regMod,
    regAnd,
    regOr,
    regXor,
    Stack (..),
    newStack,
    stackPush,
    stackPop,
    stackPeek,
    stackDup,
    stackSwap,
    stackRot,
    stackGetPointer,
    stackClear,
    Heap (..),
    newHeap,
    heapSet,
    heapGet,
    heapAlloc,
    heapFree,
    heapAllocRange,
    SymTable (..),
    newSymTable,
    symSet,
    symGet,
    symGetTotalSize,
    symGetFull,
    Labels (..),
    newLabels,
    labelSet,
    labelGet,
    labelFree,
    Flag (..),
    Flags (..),
    newFlags,
    flagSet,
    flagGet,
    Instruction (..),
    Context (..),
    newContext,
    nextUUID,
    nextUUIDValid,
    ipSet,
    ipGet,
    ipInc,
    Param (..),
    setTrueValueFromParam,
    getTrueValueFromParam,
    regNot,
    insPush,
    execSyscallWrapper,
    blockInitAllocVarSpace,
    BlockMap (..),
    Block (..),
    newBlockMap,
    blockAdd,
    blockGet,
    stackGetValueFromIndex,
    blockReplace,
    codeFromValidStateInt,
    SyscallCode (..),
    callExit,
    callEasyPrint,
    codeFromEAX,
    execSyscall,
    detectLabels,
    parseLabels,
    loadContext,
    saveContext,
    callEasyPrint,
    blockInitAllocVarSpace,
    addressDoesntExist,
    maxKey,
    symGet,
    symGetType,
    adaptValueToVarType,
    sysPrintValue,
    truePrintValue,
    labelGet,
    labelFree,
    showInstructArray,
    hasmNStackPush
  )
where

import Data.Binary (Binary, decode, decodeFile, encode, encodeFile)
import Data.Binary.Get (getWord32le, getWord64le)
import Data.Binary.Put (putWord32le, putWord64le)
import Data.Bits (Bits (complement, xor, (.&.), (.|.)))
import qualified Data.ByteString.Lazy as BS
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Lexer
import ValidState

---------------------------------------------------------------------------
-- SYSCALLS
---------------------------------------------------------------------------

data SyscallCode
  = SCExit
  | SCEasyPrint
  deriving (Show, Eq, Generic)

instance Binary SyscallCode

codeFromValidStateInt :: ValidState Int -> SyscallCode
codeFromValidStateInt (Invalid _) = SCExit
codeFromValidStateInt (Valid i) = case i of
  1 -> SCExit
  4 -> SCEasyPrint
  _ -> SCExit

callExit :: ValidState Context -> ValidState Context
callExit (Invalid s) = Invalid s
callExit (Valid ctx) = Valid ctx {exit = True}

callEasyPrint :: ValidState Context -> IO ()
callEasyPrint ctx = case getTrueValueFromParam ctx (Reg ECX) of
  (Invalid s) -> putStrLn "Register error"
  Valid val -> truePrintValue ctx (Reg EBX) (Reg ECX)

codeFromEAX :: Context -> SyscallCode
codeFromEAX ctx = codeFromValidStateInt (getTrueValueFromParam (Valid ctx) (Reg EAX))

execSyscall :: ValidState Context -> SyscallCode -> (ValidState Context, IO ())
execSyscall (Invalid s) _ = (Invalid s, putStr s)
execSyscall (Valid ctx) SCEasyPrint = (Valid ctx, callEasyPrint (Valid ctx))
execSyscall (Valid ctx) SCExit = (callExit (Valid ctx), putStr "exit")

execSyscallWrapper :: ValidState Context -> (ValidState Context, IO ())
execSyscallWrapper (Invalid s) = (Invalid s, putStr "")
execSyscallWrapper (Valid ctx) = execSyscall (Valid ctx) (codeFromEAX ctx)

-------------------------------------------------------------------------------
-- REGISTERS
-------------------------------------------------------------------------------

data Register = EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP | E8 | E9
  deriving (Eq, Ord, Show, Generic)

newtype Registers = Registers {regs :: Map.Map Register Int} deriving (Show, Eq, Generic)

instance Binary Registers

instance Binary Register

-- | Creates a new empty set of registers.
newRegisters :: Registers
newRegisters = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])

-- | Sets the value of a register.
regSet :: ValidState Context -> Register -> Int -> ValidState Context
regSet (Invalid s) _ _ = Invalid s
regSet (Valid context) register value = Valid context {registers = Registers (Map.insert register value (regs (registers context)))}

regNot :: ValidState Context -> Register -> ValidState Context
regNot (Invalid s) _ = Invalid s
regNot (Valid context) register = Valid context {registers = Registers (Map.adjust complement register (regs (registers context)))}

-- | Gets the value of a register
regGet :: ValidState Context -> Register -> ValidState Int
regGet (Invalid s) _ = Invalid s
regGet (Valid context) register = case Map.lookup register (regs (registers context)) of
  Nothing -> Invalid "Register not found"
  Just value -> Valid value

-- | Increments the value of a register.
regInc :: ValidState Context -> Register -> ValidState Context
regInc (Invalid s) _ = Invalid s
regInc (Valid context) register = Valid context {registers = Registers (Map.adjust (+ 1) register (regs (registers context)))}

-- | Decrements the value of a register.
regDec :: ValidState Context -> Register -> ValidState Context
regDec (Invalid s) _ = Invalid s
regDec (Valid context) register = Valid context {registers = Registers (Map.adjust (subtract 1) register (regs (registers context)))}

-- | Adds a value to the value of a register.
regAdd :: ValidState Context -> Register -> Int -> ValidState Context
regAdd (Invalid s) _ _ = Invalid s
regAdd (Valid context) register value = Valid context {registers = Registers (Map.adjust (+ value) register (regs (registers context)))}

-- | Subtracts a value from the value of a register.
regSub :: ValidState Context -> Register -> Int -> ValidState Context
regSub (Invalid s) _ _ = Invalid s
regSub (Valid context) register value = Valid context {registers = Registers (Map.adjust (subtract value) register (regs (registers context)))}

-- | Multiplies the value of a register by a value.
regMul :: ValidState Context -> Register -> Int -> ValidState Context
regMul (Invalid s) _ _ = Invalid s
regMul (Valid context) register value = Valid context {registers = Registers (Map.adjust (* value) register (regs (registers context)))}

-- | Divides the value of a register by a value.
regDiv :: ValidState Context -> Register -> Int -> ValidState Context
regDiv _ _ 0 = Invalid "Division by zero"
regDiv (Invalid s) _ _ = Invalid s
regDiv (Valid context) register value = Valid context {registers = Registers (Map.adjust (`div` value) register (regs (registers context)))}

-- | Modulo the value of a register by a value.
regMod :: ValidState Context -> Register -> Int -> ValidState Context
regMod _ _ 0 = Invalid "Modulo by zero"
regMod (Invalid s) _ _ = Invalid s
regMod (Valid context) register value = Valid context {registers = Registers (Map.adjust (`mod` value) register (regs (registers context)))}

-- | Bitwise AND the value of a register by a value.
regAnd :: ValidState Context -> Register -> Int -> ValidState Context
regAnd (Invalid s) _ _ = Invalid s
regAnd (Valid context) register value = Valid context {registers = Registers (Map.adjust (.&. value) register (regs (registers context)))}

-- | Bitwise OR the value of a register by a value.
regOr :: ValidState Context -> Register -> Int -> ValidState Context
regOr (Invalid s) _ _ = Invalid s
regOr (Valid context) register value = Valid context {registers = Registers (Map.adjust (.|. value) register (regs (registers context)))}

-- | Bitwise XOR the value of a register by a value.
regXor :: ValidState Context -> Register -> Int -> ValidState Context
regXor (Invalid s) _ _ = Invalid s
regXor (Valid context) register value = Valid context {registers = Registers (Map.adjust (`xor` value) register (regs (registers context)))}

-------------------------------------------------------------------------------
-- STACK
-------------------------------------------------------------------------------

newtype Stack = Stack {pile :: [Int]} deriving (Show, Eq, Generic)

instance Binary Stack

-- | Creates a new empty stack.
newStack :: Stack
newStack = Stack []

-- | Pushes a value on the stack.
stackPush :: ValidState Context -> Int -> ValidState Context
stackPush (Invalid s) _ = Invalid s
stackPush (Valid context) value = Valid context {stack = Stack (pile (stack context) ++ [value]), registers = Registers (Map.adjust (+ 1) ESP (regs (registers context)))}

-- | Pops a value from the stack.
stackPop :: ValidState Context -> ValidState (Int, ValidState Context)
stackPop (Invalid s) = Invalid s
stackPop (Valid context) = case pile (stack context) of
  [] -> Invalid "Empty stack"
  arr -> Valid (last arr, Valid context {stack = Stack (init arr), registers = Registers (Map.adjust (subtract 1) ESP (regs (registers context)))})

-- | Peeks a value from the stack.
stackPeek :: ValidState Context -> ValidState (Int, ValidState Context)
stackPeek (Invalid s) = Invalid s
stackPeek (Valid context) = case pile (stack context) of
  [] -> Invalid "Empty stack"
  (x : _) -> Valid (x, Valid context)

-- | Duplicates the top value of the stack.
stackDup :: ValidState Context -> ValidState Context
stackDup (Invalid s) = Invalid s
stackDup (Valid context) = case pile (stack context) of
  [] -> Invalid "Empty stack"
  (x : xs) -> Valid context {stack = Stack (x : x : xs)}

-- | Swaps the two top values of the stack.
stackSwap :: ValidState Context -> ValidState Context
stackSwap (Invalid s) = Invalid s
stackSwap (Valid context) = case pile (stack context) of
  [] -> Invalid "Empty stack"
  (x : y : xs) -> Valid context {stack = Stack (y : x : xs)}
  (_ : _) -> Invalid "Not enough values on the stack"

-- | Rotates the three top values of the stack.
stackRot :: ValidState Context -> ValidState Context
stackRot (Invalid s) = Invalid s
stackRot (Valid context) = case pile (stack context) of
  [] -> Invalid "Empty stack"
  (x : y : z : xs) -> Valid context {stack = Stack (z : x : y : xs)}
  (_ : _) -> Invalid "Not enough values on the stack"

stackGetPointer :: ValidState Context -> (Int, ValidState Context)
stackGetPointer (Invalid s) = (0, Invalid s)
stackGetPointer (Valid context) = (length (pile (stack context)), Valid context)

stackClear :: ValidState Context -> ValidState Context
stackClear (Invalid s) = Invalid s
stackClear (Valid context) = Valid context {stack = Stack []}

stackGetValueFromIndex :: ValidState Context -> Int -> ValidState Int
stackGetValueFromIndex (Invalid _) _ = Invalid "Invalid context"
stackGetValueFromIndex (Valid context) index = case pile (stack context) of
  [] -> Invalid "Empty stack"
  list -> case (elemIndex index list) of
    Just n -> Valid n
    Nothing -> Invalid "Value not retrieve"

--------------------------------------------------------------------------------
-- HEAP
--------------------------------------------------------------------------------

-- | The heap of the VM. It holds all the created symbols that can be referenced
-- by their address, and maps them to their value.
newtype Heap = Heap {mem :: Map.Map Int Int} deriving (Show, Eq, Generic)

instance Binary Heap

-- | Creates a new empty heap.
newHeap :: Heap
newHeap = Heap Map.empty

addressDoesntExist :: Map.Map Int Int -> Int -> Bool
addressDoesntExist m address = case Map.lookup address m of
  Nothing -> True
  Just _ -> False

-- | Sets the value of a symbol in the heap.
heapSet :: ValidState Context -> Int -> Int -> ValidState Context
heapSet (Invalid s) _ _ = Invalid s
heapSet (Valid context) address value
  | address < 0 = Invalid "Negative address"
  | Map.lookup address (mem (heap context)) == Nothing = Invalid ("Address not allocated: " ++ show address)
  | otherwise = Valid context {heap = Heap (Map.insert address value (mem (heap context)))}

-- | Gets the value of a symbol in the heap. If the address isnt allocated, it
heapGet :: ValidState Context -> Int -> ValidState Int
heapGet (Invalid s) _ = Invalid s
heapGet (Valid context) address = case Map.lookup address (mem (heap context)) of
  Nothing -> Invalid "Address not allocated"
  Just value -> Valid value

-- | Prelude.returns the maximum address of the given map.
maxKey :: Map.Map Int Int -> ValidState Int
maxKey m = case Map.keys m of
  [] -> Invalid "Empty map"
  keys -> Valid (maximum keys)

-- | Allocates a new symbol in the heap, and Prelude.returns its address. If the alloc fails, it Prelude.returns 0. (null)
heapAlloc :: ValidState Context -> ValidState (Int, ValidState Context)
heapAlloc (Invalid s) = Invalid s
heapAlloc (Valid context) = Valid (addr, Valid context {heap = Heap (Map.insert addr 0 (mem (heap context)))})
  where
    addr = case maxKey (mem (heap context)) of
      (Invalid _) -> 1
      Valid key -> key + 1

-- | Allocates a range in the memory.
heapAllocRange :: ValidState Context -> Int -> (Int, ValidState Context)
heapAllocRange (Invalid s) _ = (0, Invalid s)
heapAllocRange (Valid context) size = (addr, Valid context {heap = Heap (Map.union (Map.fromList (zip [addr .. addr + size - 1] (repeat 0))) (mem (heap context)))})
  where
    addr = case maxKey (mem (heap context)) of
      (Invalid _) -> 1
      Valid key -> key + 1

-- | Frees a symbol in the heap.
heapFree :: ValidState Context -> Int -> ValidState Context
heapFree (Invalid s) _ = Invalid s
heapFree (Valid context) address = Valid context {heap = Heap (Map.delete address (mem (heap context)))}

--------------------------------------------------------------------------------
-- SYMBOL TABLE
--------------------------------------------------------------------------------

-- | The symbol table of the VM. It holds all the created symbols that can be
-- referenced by their name, and maps them to their address in the stack.

-- | variable names with their size
newtype SymTable = SymTable {symTable :: [(String, VarType)]} deriving (Show, Eq, Generic)

instance Binary SymTable

newSymTable :: SymTable
newSymTable = SymTable []

symSet :: ValidState Context -> String -> VarType -> ValidState Context
symSet (Invalid s) _ _ = Invalid ("Failed to set symbol because the previous error was thrown: " ++ s)
symSet (Valid c) name tp = Valid c {symbolTable = SymTable (symTable (symbolTable c) ++ [(name, tp)])}

symGet :: ValidState Context -> String -> ValidState Int
symGet (Invalid s) _ = Invalid s
symGet (Valid c) name = case getSymAddress (symTable (symbolTable c)) name of
  -1 -> Invalid "Symbol not found"
  address -> Valid address

symGetType :: ValidState Context -> String -> ValidState VarType
symGetType (Invalid s) _ = Invalid s
symGetType (Valid c) name = case getSymAddress (symTable (symbolTable c)) name of
  -1 -> Invalid "Symbol not found"
  address -> Valid (snd ((symTable (symbolTable c)) !! address))

symGetFull :: ValidState Context -> String -> ValidState (String, VarType)
symGetFull (Invalid s) _ = Invalid s
symGetFull (Valid c) name = case getSymAddress (symTable (symbolTable c)) name of
  -1 -> Invalid "Symbol not found"
  address -> Valid (symTable (symbolTable c) !! address)

-- Prelude.returns the index of the element with the given name in the symbol table, or -1
-- if it doesn't exist
getSymAddress :: [(String, VarType)] -> String -> Int
getSymAddress [] _ = -1
getSymAddress ((name, _) : xs) target =
  if name == target
    then 0
    else case getSymAddress xs target of
      -1 -> -1
      address -> address + 1

symGetTotalSize :: ValidState Context -> ValidState Int
symGetTotalSize (Invalid s) = Invalid s
symGetTotalSize (Valid c) = Valid (length (symTable (symbolTable c)))

adaptValueToVarType :: VarType -> ValidState Int -> String
adaptValueToVarType _ (Invalid s) = s
adaptValueToVarType tp (Valid val) = case tp of
  GInt -> show val
  GBool -> if val == 0 then "false" else "true"
  GVoid -> ""
  GPtr -> show val
  GUndefinedType -> "Undefined"

-- Recieves the Context, list of symbols and the symbol you are looking for and prints it
sysPrintValue :: ValidState Context -> ValidState VarType -> String -> IO ()
sysPrintValue (Invalid s) _ _ = putStrLn s
sysPrintValue (Valid c) tp name = case tp of
  Invalid _ -> putStrLn "Invalid Type"
  Valid tp ->
    putStrLn
      ( adaptValueToVarType tp (stackGetValueFromIndex (Valid c) (fromValidState 0 (symGet (Valid c) name)))
      )

truePrintValue :: ValidState Context -> Param -> Param -> IO ()
truePrintValue (Invalid s) _ _ = putStrLn s
truePrintValue c varType param = putStrLn (adaptValueToVarType (fromValidState GUndefinedType trueType) (getTrueValueFromParam c param))
  where
    trueType = intToType (getTrueValueFromParam c varType)

--------------------------------------------------------------------------------
-- LABELS
--------------------------------------------------------------------------------

-- | The labels of the VM. It holds all the created labels that refer to an
-- instruction, and maps them to their address in the instruction pile.
newtype Labels = Labels {labelMap :: Map.Map String Int} deriving (Show, Eq, Generic)

instance Binary Labels

-- | Creates a new empty label pile.
newLabels :: Labels
newLabels = Labels Map.empty

-- | Sets the address of a label in the label pile.
labelSet :: ValidState Context -> String -> Int -> ValidState Context
labelSet (Invalid s) _ _ = Invalid s
labelSet (Valid context) name address = Valid context {labels = Labels (Map.insert name address (labelMap (labels context)))}

-- | Gets the address of a label in the label pile. If the label isnt
-- allocated, it Prelude.returns Invalid s.
labelGet :: ValidState Context -> String -> ValidState Int
labelGet (Invalid s) _ = Invalid s
labelGet (Valid context) name = case Map.lookup name (labelMap (labels context)) of
  Nothing -> Invalid "Label not allocated"
  Just value -> Valid value

-- | Frees a label in the label pile.
labelFree :: ValidState Context -> String -> ValidState Context
labelFree (Invalid s) _ = Invalid s
labelFree (Valid context) name = Valid context {labels = Labels (Map.delete name (labelMap (labels context)))}

--------------------------------------------------------------------------------
-- Flags
--------------------------------------------------------------------------------

-- | The flags of the VM. It holds all the flags that can be set or unset.
data Flag
  = ZF -- Zero flag
  | SF -- Sign flag
  | OF -- Overflow flag
  | CF -- Carry flag
  | PF -- Parity flag
  | AF -- Auxiliary flag
  deriving (Eq, Ord, Show, Generic)

instance Binary Flag

newtype Flags = Flags {flagMap :: Map.Map Flag Bool} deriving (Show, Eq, Generic)

instance Binary Flags

-- | Creates a new empty set of flags.
newFlags :: Flags
newFlags = Flags (Map.fromList [(ZF, False), (SF, False), (OF, False), (CF, False), (PF, False), (AF, False)])

-- | Sets the value of a flag.
flagSet :: ValidState Context -> Flag -> Bool -> ValidState Context
flagSet (Invalid s) _ _ = Invalid s
flagSet (Valid context) flag value = Valid context {flags = Flags (Map.insert flag value (flagMap (flags context)))}

-- | Gets the value of a flag.
flagGet :: ValidState Context -> Flag -> Bool
flagGet (Invalid _) _ = False
flagGet (Valid context) flag = fromMaybe False (Map.lookup flag (flagMap (flags context)))

--------------------------------------------------------------------------------
-- INSTRUCTIONS
--------------------------------------------------------------------------------

data Param
  = Reg Register
  | Immediate Int
  | Memory Int
  | Symbol String
  deriving (Eq, Ord, Show, Generic)

instance Binary Param

data Instruction
  = Mov Param Param
  | MovPtr Param Param -- mov [eax], ebx
  | DerefMacro Register -- mov eax, [reg + index]
  | MovStackAddr Param Param -- mov [ebp + 4], ebx
  | MovFromStackAddr Param Param -- mov ebx, [ebp + 4]
  | Nop
  | Push Param
  | Pop Param
  | IMul Param Param -- noop
  | Xor Param Param
  | Or Param Param
  | And Param Param
  | Not Param
  | Enter
  | Leave
  | Cmp Param Param
  | Test Param Param
  | Jmp String
  | Je String
  | Jne String
  | Js String
  | Jns String
  | Jg String
  | Jge String
  | Jl String
  | Jle String
  | Ja String
  | Jae String
  | Jb String
  | Jbe String
  | Inc Register
  | Dec Register
  | Neg Register
  | Add Register Param
  | Sub Param Param
  | Mult Param Param
  | Div Param
  | Call String -- calls evaluates a function (block)
  | Interrupt
  | Label String Int -- name of the label, instruction pointer at the time.
  | Ret
-- macros
  | Alloc Int
  | ShowInt
  | ShowBool
  | Write Int Param Int
  deriving (Eq, Show, Generic)

instance Binary Instruction

-- | Prelude.returns the real value contained after resolving the param.
-- For exemple if the param is a register, this function will Prelude.return the value
-- contained in the register. An immediate will Prelude.return its value, Memory will
-- Prelude.return the value contained at the address in the heap, and Symbol will Prelude.return
-- the value contained in the symbol table.
getTrueValueFromParam :: ValidState Context -> Param -> ValidState Int
getTrueValueFromParam (Invalid s) _ = Invalid s
getTrueValueFromParam (Valid context) param = case param of
  Reg register -> regGet (Valid context) register
  Immediate value -> Valid value
  Memory address -> heapGet (Valid context) address
  Symbol name -> case symGet (Valid context) name of
    (Invalid s) -> Invalid s
    Valid address -> heapGet (Valid context) address

setTrueValueFromParam :: ValidState Context -> Param -> Int -> ValidState Context
setTrueValueFromParam (Invalid s) _ _ = Invalid s
setTrueValueFromParam (Valid context) param value = case param of
  Reg register -> regSet (Valid context) register value
  Immediate _ -> Invalid "Cannot set value of an immediate type parameter"
  Memory address -> heapSet (Valid context) address value
  Symbol name -> case symGet (Valid context) name of
    (Invalid s) -> Invalid s
    Valid address -> heapSet (Valid context) address value

-------------------------------------------------------------------------------
-- BLOCKS
-------------------------------------------------------------------------------

data Block = Block
  { blockName :: String,
    blockContext :: ValidState Context,
    blockParamTypes :: [VarType]
  }
  deriving (Show, Eq, Generic)

instance Binary Block

newtype BlockMap = BlockMap {blockMap :: Map.Map String Block} deriving (Show, Eq, Generic)

instance Binary BlockMap

newBlockMap :: BlockMap
newBlockMap = BlockMap Map.empty

blockAdd :: ValidState Context -> String -> ValidState Context
blockAdd (Invalid s) _ = Invalid s
blockAdd (Valid c) name = case Map.lookup name (blockMap (blocks c)) of
  Just _ -> Invalid ("Block already defined: " ++ name)
  Nothing -> Valid c {blocks = BlockMap (Map.insert name (Block name (Valid newContext) []) (blockMap (blocks c)))}

blockReplace :: ValidState Context -> ValidState Block -> ValidState Context
blockReplace (Invalid s) _ = Invalid s
blockReplace _ (Invalid s) = Invalid s
blockReplace (Valid c) (Valid block) =
  let name = blockName block
   in case Map.lookup name (blockMap (blocks c)) of
        Just _ -> Valid c {blocks = BlockMap (Map.insert name block (blockMap (blocks c)))}
        Nothing -> Invalid ("Block not found: " ++ name)

blockGet :: ValidState Context -> String -> ValidState Block
blockGet (Invalid s) _ = Invalid s
blockGet (Valid c) name = case Map.lookup name (blockMap (blocks c)) of
  Just block -> Valid block
  Nothing -> Invalid ("Block not found: " ++ name)

--------------------------------------------------------------------------------
-- CONTEXT
--------------------------------------------------------------------------------

-- | The context of the VM.
-- It contains the registries, the stack, the heap, and a pile of instructions.
data Context = Context
  { registers :: Registers,
    stack :: Stack,
    heap :: Heap,
    instructions :: [Instruction],
    symbolTable :: SymTable,
    labels :: Labels,
    flags :: Flags,
    instructionPointer :: Int,
    exit :: Bool,
    uuids :: Int,
    cAST :: [ASTNode],
    blocks :: BlockMap
  }
  deriving (Eq, Generic)

instance Binary Context

saveContext :: ValidState Context -> String -> IO ()
saveContext (Invalid s) _ = putStrLn s
saveContext (Valid c) filepath = BS.writeFile filepath (encode (c {cAST = []}))

loadContext :: String -> IO (ValidState Context)
loadContext filepath = do
  file <- BS.readFile filepath
  Prelude.return (Valid (decode file))

showInstructArray :: [Instruction] -> String
showInstructArray [] = ""
showInstructArray (x : xs) = show x ++ "\n" ++ showInstructArray xs

instance Show Context where
  show (Context r s h i sym l f iP e u a bm) =
    "Context {registers = " ++ show r ++ ", stack = " ++ show s ++ ", heap = " ++ show h ++ ", instructions = [\n" ++ showInstructArray i ++ "\n], symbolTable = " ++ show sym ++ ", labels = " ++ show l ++ ", flags = " ++ show f ++ ", instructionPointer = " ++ show iP ++ ", exit = " ++ show e ++ ", uuids = " ++ show u ++ show a ++ show bm ++ "}"

-- | Creates a new empty context.
newContext :: Context
newContext = Context newRegisters newStack newHeap [] newSymTable newLabels newFlags 0 False 0 [] newBlockMap

nextUUID :: Context -> (Int, Context)
nextUUID context = (uuids context, context {uuids = uuids context + 1})

nextUUIDValid :: ValidState Context -> (Int, Context)
nextUUIDValid (Invalid s) = (0, newContext)
nextUUIDValid (Valid context) = (uuids context, context {uuids = uuids context + 1})

-- | Sets the value of the instruction pointer.
ipSet :: ValidState Context -> Int -> ValidState Context
ipSet (Invalid s) _ = Invalid s
ipSet (Valid context) value =
  if value < 0
    then Invalid "Invalid instruction pointer value"
    else Valid context {instructionPointer = value}

-- | Gets the value of the instruction pointer.
ipGet :: ValidState Context -> ValidState Int
ipGet (Invalid s) = Invalid s
ipGet (Valid context) = Valid (instructionPointer context)

-- | Increments the value of the instruction pointer.
ipInc :: ValidState Context -> ValidState Context
ipInc (Invalid s) = Invalid s
ipInc (Valid context) =
  if instructionPointer context + 1 > length (instructions context)
    then Invalid "Invalid instruction pointer value"
    else Valid context {instructionPointer = instructionPointer context + 1}

-- | Push instruction on the ins pile
insPush :: ValidState Context -> Instruction -> ValidState Context
insPush (Invalid s) _ = Invalid s
insPush (Valid context) instruction = Valid context {instructions = instruction : instructions context}

hasmNStackPush :: Int -> [Instruction]
hasmNStackPush 0 = []
hasmNStackPush n = Push (Immediate 0) : hasmNStackPush (n - 1)

blockInitAllocVarSpace :: ValidState Context -> [Instruction]
blockInitAllocVarSpace (Invalid _) = []
blockInitAllocVarSpace (Valid c) =
  Enter : hasmNStackPush neededSpace
  where
    neededSpace = length (symTable (symbolTable c)) - length (pile (stack c))

parseLabels :: ValidState Context -> [Instruction] -> Int -> ValidState Context
parseLabels (Invalid s) _ _ = Invalid s
parseLabels (Valid c) [] _ = Valid c
parseLabels (Valid c) (i : is) idx = case i of
  Label name _ -> parseLabels (labelSet (Valid c) name idx) is (idx + 1)
  _ -> parseLabels (Valid c) is (idx + 1)

-- detects the labels in the instructions and adds them to the context
detectLabels :: ValidState Context -> ValidState Context
detectLabels (Invalid s) = Invalid s
detectLabels (Valid c) = parseLabels (Valid c) (instructions c) 0

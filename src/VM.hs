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
    , stackGetPointer
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
    ipSet,
    ipGet,
    ipInc,
    Param (..),
    setTrueValueFromParam,
    getTrueValueFromParam,
    regNot,
    insPush,
    execSyscallWrapper,
    blockInitAllocVarSpace
  )
where

import Data.Bits
import qualified Data.Map as Map
import Data.Maybe

---------------------------------------------------------------------------
-- SYSCALLS
---------------------------------------------------------------------------


-- when adding a syscall value don't forget to add it to the codeFromMaybeInt function
-- just below
data SyscallCode = SCExit
                    | SCEasyPrint
                    deriving (Show, Eq, Ord)


codeFromMaybeInt :: Maybe Int -> SyscallCode
codeFromMaybeInt Nothing = SCExit
codeFromMaybeInt (Just i) = case i of
    1 -> SCExit
    123456789 -> SCEasyPrint -- this is not a real syscall
    _ -> SCExit

callExit :: Maybe Context -> Maybe Context
callExit Nothing = Nothing
callExit (Just ctx) = Just ctx { exit = True }

callEasyPrint :: Maybe Context -> IO()
callEasyPrint ctx = case getTrueValueFromParam ctx (Reg EAX) of
    Nothing -> putStrLn "Register error"
    Just val -> print val


-- | Gets a syscall Code from EAX (int) returns the SyscallCode value associated
codeFromEAX :: Context -> SyscallCode
codeFromEAX ctx = codeFromMaybeInt (getTrueValueFromParam (Just ctx) (Reg EAX))

-- | executes a syscall from its code. (use SyscallCode datatype)
execSyscall :: Maybe Context -> SyscallCode -> (Maybe Context, Maybe (IO()))
execSyscall Nothing _ = (Nothing, Nothing)
-- printf
execSyscall (Just ctx) SCEasyPrint = (Just ctx, Just (callEasyPrint (Just ctx)))
-- exit
execSyscall (Just ctx) SCExit = (callExit (Just ctx), Nothing)

execSyscallWrapper :: Maybe Context -> Maybe Context
execSyscallWrapper Nothing = Nothing
execSyscallWrapper (Just ctx) = fst (execSyscall (Just ctx) (codeFromEAX ctx))


-------------------------------------------------------------------------------
-- REGISTERS
-------------------------------------------------------------------------------

-- | The registers of the VM. Cf assembly registers.
data Register = EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP
  deriving (Eq, Ord, Show)

newtype Registers = Registers { regs:: Map.Map Register Int } deriving (Show, Eq)

-- | Creates a new empty set of registers.
newRegisters :: Registers
newRegisters = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])

-- | Sets the value of a register.
regSet :: Maybe Context -> Register -> Int -> Maybe Context
regSet Nothing _ _ = Nothing
regSet (Just context) register value = Just context {registers = Registers (Map.insert register value (regs (registers context)))}

regNot :: Maybe Context -> Register -> Maybe Context
regNot Nothing _ = Nothing
regNot (Just context) register = Just context {registers = Registers (Map.adjust complement register (regs (registers context)))}

-- | Gets the value of a register.
regGet :: Maybe Context -> Register -> Maybe Int
regGet Nothing _ = Nothing
regGet (Just context) register = Map.lookup register (regs (registers context))

-- | Increments the value of a register.
regInc :: Maybe Context -> Register -> Maybe Context
regInc Nothing _ = Nothing
regInc (Just context) register = Just context {registers = Registers (Map.adjust (+ 1) register (regs (registers context)))}

-- | Decrements the value of a register.
regDec :: Maybe Context -> Register -> Maybe Context
regDec Nothing _ = Nothing
regDec (Just context) register = Just context {registers = Registers (Map.adjust (subtract 1) register (regs (registers context)))}

-- | Adds a value to the value of a register.
regAdd :: Maybe Context -> Register -> Int -> Maybe Context
regAdd Nothing _ _ = Nothing
regAdd (Just context) register value = Just context {registers = Registers (Map.adjust (+ value) register (regs (registers context)))}

-- | Subtracts a value from the value of a register.
regSub :: Maybe Context -> Register -> Int -> Maybe Context
regSub Nothing _ _ = Nothing
regSub (Just context) register value = Just context {registers = Registers (Map.adjust (subtract value) register (regs (registers context)))}

-- | Multiplies the value of a register by a value.
regMul :: Maybe Context -> Register -> Int -> Maybe Context
regMul Nothing _ _ = Nothing
regMul (Just context) register value = Just context {registers = Registers (Map.adjust (* value) register (regs (registers context)))}

-- | Divides the value of a register by a value.
regDiv :: Maybe Context -> Register -> Int -> Maybe Context
regDiv _ _ 0 = Nothing
regDiv Nothing _ _ = Nothing
regDiv (Just context) register value = Just context {registers = Registers (Map.adjust (`div` value) register (regs (registers context)))}

-- | Modulo the value of a register by a value.
regMod :: Maybe Context -> Register -> Int -> Maybe Context
regMod _ _ 0 = Nothing
regMod Nothing _ _ = Nothing
regMod (Just context) register value = Just context {registers = Registers (Map.adjust (`mod` value) register (regs (registers context)))}

-- | Bitwise AND the value of a register by a value.
regAnd :: Maybe Context -> Register -> Int -> Maybe Context
regAnd Nothing _ _ = Nothing
regAnd (Just context) register value = Just context {registers = Registers (Map.adjust (.&. value) register (regs (registers context)))}

-- | Bitwise OR the value of a register by a value.
regOr :: Maybe Context -> Register -> Int -> Maybe Context
regOr Nothing _ _ = Nothing
regOr (Just context) register value = Just context {registers = Registers (Map.adjust (.|. value) register (regs (registers context)))}

-- | Bitwise XOR the value of a register by a value.
regXor :: Maybe Context -> Register -> Int -> Maybe Context
regXor Nothing _ _ = Nothing
regXor (Just context) register value = Just context {registers = Registers (Map.adjust (`xor` value) register (regs (registers context)))}

-------------------------------------------------------------------------------
-- STACK
-------------------------------------------------------------------------------

newtype Stack = Stack {pile :: [Int]} deriving (Show, Eq)

-- | Creates a new empty stack.
newStack :: Stack
newStack = Stack []

-- | Pushes a value on the stack.
stackPush :: Maybe Context -> Int -> Maybe Context
stackPush Nothing _ = Nothing
stackPush (Just context) value = Just context {stack = Stack (value : pile (stack context))}

-- | Pops a value from the stack.
stackPop :: Maybe Context -> Maybe (Int, Maybe Context)
stackPop Nothing = Nothing
stackPop (Just context) = case pile (stack context) of
  [] -> Nothing
  (x : xs) -> Just (x, Just context {stack = Stack xs})

-- | Peeks a value from the stack.
stackPeek :: Maybe Context -> Maybe (Int, Maybe Context)
stackPeek Nothing = Nothing
stackPeek (Just context) = case pile (stack context) of
  [] -> Nothing
  (x : _) -> Just (x, Just context)

-- | Duplicates the top value of the stack.
stackDup :: Maybe Context -> Maybe Context
stackDup Nothing = Nothing
stackDup (Just context) = case pile (stack context) of
  [] -> Nothing
  (x : xs) -> Just context {stack = Stack (x : x : xs)}

-- | Swaps the two top values of the stack.
stackSwap :: Maybe Context -> Maybe Context
stackSwap Nothing = Nothing
stackSwap (Just context) = case pile (stack context) of
  [] -> Nothing
  (x : y : xs) -> Just context {stack = Stack (y : x : xs)}
  (_ : _) -> Nothing

-- | Rotates the three top values of the stack.
stackRot :: Maybe Context -> Maybe Context
stackRot Nothing = Nothing
stackRot (Just context) = case pile (stack context) of
  [] -> Nothing
  (x : y : z : xs) -> Just context {stack = Stack (z : x : y : xs)}
  (_ : _) -> Nothing

stackGetPointer :: Maybe Context -> (Int, Maybe Context)
stackGetPointer Nothing = (0, Nothing)
stackGetPointer (Just context) = (length (pile (stack context)), Just context)

--------------------------------------------------------------------------------
-- HEAP
--------------------------------------------------------------------------------

-- | The heap of the VM. It holds all the created symbols that can be referenced
-- by their address, and maps them to their value.
newtype Heap = Heap {mem :: Map.Map Int Int} deriving (Show, Eq)

-- | Creates a new empty heap.
newHeap :: Heap
newHeap = Heap Map.empty

addressDoesntExist :: Map.Map Int Int -> Int -> Bool
addressDoesntExist m address = case Map.lookup address m of
  Nothing -> True
  Just _ -> False

-- | Sets the value of a symbol in the heap.
-- @params:
--     context: the context of the VM
--     address: the address of the symbol
--     value: the value to set
-- @return: the new context, or Nothing if the address is negative or if the
-- address is not allocated.
heapSet :: Maybe Context -> Int -> Int -> Maybe Context
heapSet Nothing _ _ = Nothing
heapSet (Just context) address value
  | address < 0 = Nothing
  | Map.lookup address (mem (heap context)) == Nothing = Nothing
  | otherwise = Just context {heap = Heap (Map.insert address value (mem (heap context)))}

-- | Gets the value of a symbol in the heap. If the address isnt allocated, it
-- returns Nothing.
heapGet :: Maybe Context -> Int -> Maybe Int
heapGet Nothing _ = Nothing
heapGet (Just context) address = Map.lookup address (mem (heap context))

-- | Returns the maximum address of the given map.
maxKey :: Map.Map Int Int -> Maybe Int
maxKey m = case Map.keys m of
  [] -> Nothing
  keys -> Just (maximum keys)

-- | Allocates a new symbol in the heap, and returns its address. If the alloc fails, it returns 0. (null)
heapAlloc :: Maybe Context -> Maybe (Int, Maybe Context)
heapAlloc Nothing = Nothing
heapAlloc (Just context) = Just (addr, Just context {heap = Heap (Map.insert addr 0 (mem (heap context)))})
  where
    addr = case maxKey (mem (heap context)) of
      Nothing -> 1
      Just key -> key + 1

-- | Allocates a range in the memory.
heapAllocRange :: Maybe Context -> Int -> (Int, Maybe Context)
heapAllocRange Nothing _ = (0, Nothing)
heapAllocRange (Just context) size = (addr, Just context {heap = Heap (Map.union (Map.fromList (zip [addr .. addr + size - 1] (repeat 0))) (mem (heap context)))})
  where
    addr = case maxKey (mem (heap context)) of
      Nothing -> 1
      Just key -> key + 1

-- | Frees a symbol in the heap.
heapFree :: Maybe Context -> Int -> Maybe Context
heapFree Nothing _ = Nothing
heapFree (Just context) address = Just context {heap = Heap (Map.delete address (mem (heap context)))}

--------------------------------------------------------------------------------
-- SYMBOL TABLE
--------------------------------------------------------------------------------

-- | The symbol table of the VM. It holds all the created symbols that can be
-- referenced by their name, and maps them to their address in the stack.
-- newtype SymTable = SymTable {symTable :: Map.Map String Int} deriving (Show, Eq)

-- -- | Creates a new empty symbol table.
-- newSymTable :: SymTable
-- newSymTable = SymTable Map.empty

-- -- | Sets the address of a symbol in the symbol table.
-- symSet :: Maybe Context -> String -> Int -> Maybe Context
-- symSet Nothing _ _ = Nothing
-- symSet (Just context) name address = Just context {symbolTable = SymTable (Map.insert name address (symTable (symbolTable context)))}

-- -- | Gets the address of a symbol in the symbol table. If the symbol isnt
-- -- allocated, it returns Nothing.
-- symGet :: Maybe Context -> String -> Maybe Int
-- symGet Nothing _ = Nothing
-- symGet (Just context) name = Map.lookup name (symTable (symbolTable context))

-- -- | Allocates a new symbol in the symbol table, and returns its address.
-- symAlloc :: Maybe Context -> String -> Maybe Context
-- symAlloc Nothing _ = Nothing
-- symAlloc (Just context) name = Just context {symbolTable = SymTable (Map.insert name (length (mem (heap context))) (symTable (symbolTable context)))}

-- -- | Frees a symbol in the symbol table.
-- symFree :: Maybe Context -> String -> Maybe Context
-- symFree Nothing _ = Nothing
-- symFree (Just context) name = Just context {symbolTable = SymTable (Map.delete name (symTable (symbolTable context)))}

-- | variable names with their size
newtype SymTable = SymTable {symTable :: [(String, Int)]} deriving (Show, Eq)

newSymTable :: SymTable
newSymTable = SymTable []

symSet :: Maybe Context -> String -> Int -> Maybe Context
symSet Nothing _ _ = Nothing
symSet (Just c) name size = Just c {symbolTable = SymTable (symTable (symbolTable c) ++ [(name, size)])}

symGet :: Maybe Context -> String -> Maybe Int
symGet Nothing _ = Nothing
symGet (Just c) name = getSymAddress (symTable (symbolTable c)) name

getSymAddress :: [(String, Int)] -> String -> Maybe Int
getSymAddress [] _ = Nothing
getSymAddress ((name, size) : xs) target = if name == target
    then Just size
    else case getSymAddress xs target of
        Nothing -> Nothing
        Just address -> Just (address + size)

symGetTotalSize :: Maybe Context -> Maybe Int
symGetTotalSize Nothing = Nothing
symGetTotalSize (Just c) = Just (sum (map snd (symTable (symbolTable c))))

--------------------------------------------------------------------------------
-- LABELS
--------------------------------------------------------------------------------

-- | The labels of the VM. It holds all the created labels that refer to an
-- instruction, and maps them to their address in the instruction pile.
newtype Labels = Labels {labelMap :: Map.Map String Int} deriving (Show, Eq)

-- | Creates a new empty label pile.
newLabels :: Labels
newLabels = Labels Map.empty

-- | Sets the address of a label in the label pile.
labelSet :: Maybe Context -> String -> Int -> Maybe Context
labelSet Nothing _ _ = Nothing
labelSet (Just context) name address = Just context {labels = Labels (Map.insert name address (labelMap (labels context)))}

-- | Gets the address of a label in the label pile. If the label isnt
-- allocated, it returns Nothing.
labelGet :: Maybe Context -> String -> Maybe Int
labelGet Nothing _ = Nothing
labelGet (Just context) name = Map.lookup name (labelMap (labels context))

-- | Frees a label in the label pile.
labelFree :: Maybe Context -> String -> Maybe Context
labelFree Nothing _ = Nothing
labelFree (Just context) name = Just context {labels = Labels (Map.delete name (labelMap (labels context)))}

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
  deriving (Eq, Ord, Show)
data Flag
  = ZF -- Zero flag
  | SF -- Sign flag
  | OF -- Overflow flag
  | CF -- Carry flag
  | PF -- Parity flag
  | AF -- Auxiliary flag
  deriving (Eq, Ord, Show)

newtype Flags = Flags {flagMap :: Map.Map Flag Bool} deriving (Show, Eq)

-- | Creates a new empty set of flags.
newFlags :: Flags
newFlags = Flags (Map.fromList [(ZF, False), (SF, False), (OF, False), (CF, False), (PF, False), (AF, False)])

-- | Sets the value of a flag.
flagSet :: Maybe Context -> Flag -> Bool -> Maybe Context
flagSet Nothing _ _ = Nothing
flagSet (Just context) flag value = Just context {flags = Flags (Map.insert flag value (flagMap (flags context)))}

-- | Gets the value of a flag.
flagGet :: Maybe Context -> Flag -> Bool
flagGet Nothing _ = False
flagGet (Just context) flag = case Map.lookup flag (flagMap (flags context)) of
  Nothing -> False
  Just value -> value

--------------------------------------------------------------------------------
-- INSTRUCTIONS
--------------------------------------------------------------------------------

data Param
  = Reg Register
  | Immediate Int
  | Memory Int
  | Symbol String
  deriving (Eq, Ord, Show)

data Instruction
  = Mov Param Param
  | MovPtr Param Param -- mov [eax], ebx
  | MovStackAddr Param Param -- mov [ebp + 4], ebx
  | Nop
  | Push Param
  | Pop Param
  | IMul Param Param
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
  | Interrupt
  | Label String Int -- name of the label, instruction pointer at the time.
  deriving (Eq, Ord, Show)

-- | Returns the real value contained after resolving the param.
-- For exemple if the param is a register, this function will return the value
-- contained in the register. An immediate will return its value, Memory will
-- return the value contained at the address in the heap, and Symbol will return
-- the value contained in the symbol table.
getTrueValueFromParam :: Maybe Context -> Param -> Maybe Int
getTrueValueFromParam Nothing _ = Nothing
getTrueValueFromParam (Just context) param = case param of
  Reg register -> regGet (Just context) register
  Immediate value -> Just value
  Memory address -> heapGet (Just context) address
  Symbol name -> case symGet (Just context) name of
    Nothing -> Nothing
    Just address -> heapGet (Just context) address

setTrueValueFromParam :: Maybe Context -> Param -> Int -> Maybe Context
setTrueValueFromParam Nothing _ _ = Nothing
setTrueValueFromParam (Just context) param value = case param of
  Reg register -> regSet (Just context) register value
  Immediate _ -> Nothing
  Memory address -> heapSet (Just context) address value
  Symbol name -> case symGet (Just context) name of
    Nothing -> Nothing
    Just address -> heapSet (Just context) address value

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
    uuids :: Int
  }
  deriving (Show, Eq)

-- | Creates a new empty context.
newContext :: Context
newContext = Context newRegisters newStack newHeap [] newSymTable newLabels newFlags 0 False 0

nextUUID :: Context -> (Int, Context)
nextUUID context = (uuids context, context {uuids = uuids context + 1})

-- | Sets the value of the instruction pointer.
ipSet :: Maybe Context -> Int -> Maybe Context
ipSet Nothing _ = Nothing
ipSet (Just context) value =
  --   if value < 0 || value >= length (instructions context) -- to decoment when we have the real instructions count
  if value < 0
  --   if value < 0 || value >= length (instructions context) -- to decoment when we have the real instructions count
  if value < 0
    then Nothing
    else Just context {instructionPointer = value}

-- | Gets the value of the instruction pointer.
ipGet :: Maybe Context -> Maybe Int
ipGet Nothing = Nothing
ipGet (Just context) = Just (instructionPointer context)

-- | Increments the value of the instruction pointer.
ipInc :: Maybe Context -> Maybe Context
ipInc Nothing = Nothing
ipInc (Just context) =
  if instructionPointer context + 1 > length (instructions context)
    then Nothing
    else Just context {instructionPointer = instructionPointer context + 1}

-- | Executes the next instruction.
-- TODO CALL THE ACTUAL INSTRUCTION
-- TODO ADD NEW INSTRUCTION TO THE PILE
-- TODO ADD NEW INSTRUCTION TO THE PILE
-- ipNext :: Maybe Context -> Maybe Context
-- ipNext Nothing = Nothing
-- ipNext (Just context) = case instructionPointer context + 1 >= length (instructions context) of
--     True -> Nothing
--     False -> Just context { instructionPointer = instructionPointer context + 1 }

-- | Evaluates one instruction and returns the resulting context. Does not increase the instruction count.
evalOneInstruction :: Context -> Instruction -> Maybe Context
evalOneInstruction _ _ = Nothing

-- | Executes all the instructions until the instruction pointer reaches the end of the program.
-- Increases the instruction pointer after each call.
-- execInstructions :: Maybe Context -> Maybe Context
-- execInstructions Nothing = Nothing
-- execInstructions ctx = execInstructions (ipInc c)
--   where
--     c = case ctx of
--       Nothing -> Nothing
--       Just context ->
--         if instructionPointer context + 1 >= length (instructions context)
--           then Nothing
--           else evalOneInstruction context (instructions context !! instructionPointer context)

execInstructions :: Maybe Context -> Maybe Context
execInstructions Nothing = Nothing
execInstructions (Just ctx) | exit ctx = Just ctx
                            | instructionPointer ctx + 1 >= length (instructions ctx) = Nothing
                            | otherwise = execInstructions (evalOneInstruction ctx (instructions ctx !! instructionPointer ctx))
-- | Push instruction on the ins pile
insPush :: Maybe Context -> Instruction -> Maybe Context
insPush Nothing _ = Nothing
insPush (Just context) instruction = Just context {instructions = instruction : instructions context}

-- | @helps: allocates space in the stack for the block's variables using the information
-- in the symbol table. Moves ESP accordingly. The generated instructions should be used
-- before using the instructions in the block.
-- It is equivalent to :
-- push ebp
-- mov ebp, esp
-- sub esp, <size the space needed for all variables>
blockInitAllocVarSpace :: Maybe Context -> [Instruction]
blockInitAllocVarSpace Nothing = []
blockInitAllocVarSpace (Just c) = [
    Push (Reg EBP),
    Mov (Reg EBP) (Reg ESP),
    Sub (Reg ESP) (Immediate totalsize)]
    where
        totalsize = fromMaybe 0 (symGetTotalSize (Just c))


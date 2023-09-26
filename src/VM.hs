module VM (
    Register(..)
    , Registers(..)
    , newRegisters
    , regSet
    , regGet
    , regInc
    , regDec
    , regAdd
    , regSub
    , regMul
    , regDiv
    , regMod
    , regAnd
    , regOr
    , regXor
    , regNot
    , Stack(..)
    , newStack
    , stackPush
    , stackPop
    , stackPeek
    , stackDup
    , stackSwap
    , stackRot
    , Heap(..)
    , newHeap
    , heapSet
    , heapGet
    , heapAlloc
    , heapFree
    , SymTable(..)
    , newSymTable
    , symSet
    , symGet
    , symAlloc
    , symFree
    , Labels(..)
    , newLabels
    , labelSet
    , labelGet
    , labelFree
    , Flag(..)
    , Flags(..)
    , newFlags
    , flagSet
    , flagGet
    , ParamType(..)
    , Param(..)
    , Instruction(..)
    , Context(..)
    , newContext
    , ipSet
    , ipGet
    , ipInc) where

import qualified Data.Map as Map
import Data.Bits

-------------------------------------------------------------------------------
-- REGISTERS
-------------------------------------------------------------------------------

-- | The registers of the VM. Cf assembly registers.
data Register = EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP
    deriving (Eq, Ord, Show)

newtype Registers = Registers { regs:: Map.Map Register Int } deriving (Show)

-- | Creates a new empty set of registers.
newRegisters :: Registers
newRegisters = Registers (Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0), (ESI, 0), (EDI, 0), (EBP, 0), (ESP, 0)])

-- | Sets the value of a register.
regSet :: Maybe Context -> Register -> Int -> Maybe Context
regSet Nothing _ _ = Nothing
regSet (Just context) register value = Just context { registers = Registers (Map.insert register value (regs (registers context))) }

-- | Gets the value of a register.
regGet :: Maybe Context -> Register -> Maybe Int
regGet Nothing _ = Nothing
regGet (Just context) register = Map.lookup register (regs (registers context))

-- | Increments the value of a register.
regInc :: Maybe Context -> Register -> Maybe Context
regInc Nothing _ = Nothing
regInc (Just context) register = Just context { registers = Registers (Map.adjust (+1) register (regs (registers context))) }

-- | Decrements the value of a register.
regDec :: Maybe Context -> Register -> Maybe Context
regDec Nothing _ = Nothing
regDec (Just context) register = Just context { registers = Registers (Map.adjust (subtract 1) register (regs (registers context))) }

-- | Adds a value to the value of a register.
regAdd :: Maybe Context -> Register -> Int -> Maybe Context
regAdd Nothing _ _ = Nothing
regAdd (Just context) register value = Just context { registers = Registers (Map.adjust (+value) register (regs (registers context))) }

-- | Subtracts a value from the value of a register.
regSub :: Maybe Context -> Register -> Int -> Maybe Context
regSub Nothing _ _ = Nothing
regSub (Just context) register value = Just context { registers = Registers (Map.adjust (subtract value) register (regs (registers context))) }

-- | Multiplies the value of a register by a value.
regMul :: Maybe Context -> Register -> Int -> Maybe Context
regMul Nothing _ _ = Nothing
regMul (Just context) register value = Just context { registers = Registers (Map.adjust (*value) register (regs (registers context))) }

-- | Divides the value of a register by a value.
regDiv :: Maybe Context -> Register -> Int -> Maybe Context
regDiv _ _ 0 = Nothing
regDiv Nothing _ _ = Nothing
regDiv (Just context) register value = Just context { registers = Registers (Map.adjust (`div` value) register (regs (registers context))) }

-- | Modulo the value of a register by a value.
regMod :: Maybe Context -> Register -> Int -> Maybe Context
regMod _ _ 0 = Nothing
regMod Nothing _ _ = Nothing
regMod (Just context) register value = Just context { registers = Registers (Map.adjust (`mod` value) register (regs (registers context))) }

-- | Bitwise AND the value of a register by a value.
regAnd :: Maybe Context -> Register -> Int -> Maybe Context
regAnd Nothing _ _ = Nothing
regAnd (Just context) register value = Just context { registers = Registers (Map.adjust (.&. value) register (regs (registers context))) }

-- | Bitwise OR the value of a register by a value.
regOr :: Maybe Context -> Register -> Int -> Maybe Context
regOr Nothing _ _ = Nothing
regOr (Just context) register value = Just context { registers = Registers (Map.adjust (.|. value) register (regs (registers context))) }

-- | Bitwise XOR the value of a register by a value.
regXor :: Maybe Context -> Register -> Int -> Maybe Context
regXor Nothing _ _ = Nothing
regXor (Just context) register value = Just context { registers = Registers (Map.adjust (`xor` value) register (regs (registers context))) }

-- | Bitwise NOT the value of a register.
regNot :: Maybe Context -> Register -> Maybe Context
regNot Nothing _ = Nothing
regNot (Just context) register = Just context { registers = Registers (Map.adjust complement register (regs (registers context))) }

-------------------------------------------------------------------------------
-- STACK
-------------------------------------------------------------------------------

newtype Stack = Stack { pile :: [Int] } deriving (Show)

-- | Creates a new empty stack.
newStack :: Stack
newStack = Stack []

-- | Pushes a value on the stack.
stackPush :: Maybe Context -> Int -> Maybe Context
stackPush Nothing _ = Nothing
stackPush (Just context) value = Just context { stack = Stack (value : pile (stack context)) }

-- | Pops a value from the stack.
stackPop :: Maybe Context -> Maybe (Int, Maybe Context)
stackPop Nothing = Nothing
stackPop (Just context) = case pile (stack context) of
    [] -> Nothing
    (x:xs) -> Just (x, Just context { stack = Stack xs })

-- | Peeks a value from the stack.
stackPeek :: Maybe Context -> Maybe (Int, Maybe Context)
stackPeek Nothing = Nothing
stackPeek (Just context) = case pile (stack context) of
    [] -> Nothing
    (x:_) -> Just (x, Just context)

-- | Duplicates the top value of the stack.
stackDup :: Maybe Context -> Maybe Context
stackDup Nothing = Nothing
stackDup (Just context) = case pile (stack context) of
    [] -> Nothing
    (x:xs) -> Just context { stack = Stack (x:x:xs) }

-- | Swaps the two top values of the stack.
stackSwap :: Maybe Context -> Maybe Context
stackSwap Nothing = Nothing
stackSwap (Just context) = case pile (stack context) of
    [] -> Nothing
    (x:y:xs) -> Just context { stack = Stack (y:x:xs) }
    (_:_) -> Nothing

-- | Rotates the three top values of the stack.
stackRot :: Maybe Context -> Maybe Context
stackRot Nothing = Nothing
stackRot (Just context) = case pile (stack context) of
    [] -> Nothing
    (x:y:z:xs) -> Just context { stack = Stack (z:x:y:xs) }
    (_:_) -> Nothing


--------------------------------------------------------------------------------
-- HEAP
--------------------------------------------------------------------------------

-- | The heap of the VM. It holds all the created symbols that can be referenced
-- by their address, and maps them to their value.
newtype Heap = Heap { mem :: Map.Map Int Int } deriving (Show)

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
heapSet (Just context) address value | address < 0 = Nothing
    | addressDoesntExist (mem (heap context)) address = Nothing
    | otherwise = Just context { heap = Heap (Map.insert address value (mem (heap context)))}

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
heapAlloc (Just context) = Just (addr, Just context { heap = Heap (Map.insert addr 0 (mem (heap context))) })
    where addr = case maxKey (mem (heap context)) of
            Nothing -> 1
            Just key -> key + 1

-- | Frees a symbol in the heap.
heapFree :: Maybe Context -> Int -> Maybe Context
heapFree Nothing _ = Nothing
heapFree (Just context) address = Just context { heap = Heap (Map.delete address (mem (heap context))) }

--------------------------------------------------------------------------------
-- SYMBOL TABLE
--------------------------------------------------------------------------------

-- | The symbol table of the VM. It holds all the created symbols that can be
-- referenced by their name, and maps them to their address in the heap.
newtype SymTable = SymTable { symTable :: Map.Map String Int } deriving (Show)

-- | Creates a new empty symbol table.
newSymTable :: SymTable
newSymTable = SymTable Map.empty

-- | Sets the address of a symbol in the symbol table.
symSet :: Maybe Context -> String -> Int -> Maybe Context
symSet Nothing _ _ = Nothing
symSet (Just context) name address = Just context { symbolTable = SymTable (Map.insert name address (symTable (symbolTable context))) }

-- | Gets the address of a symbol in the symbol table. If the symbol isnt
-- allocated, it returns Nothing.
symGet :: Maybe Context -> String -> Maybe Int
symGet Nothing _ = Nothing
symGet (Just context) name = Map.lookup name (symTable (symbolTable context))

-- | Allocates a new symbol in the symbol table, and returns its address.
symAlloc :: Maybe Context -> String -> Maybe Context
symAlloc Nothing _ = Nothing
symAlloc (Just context) name = Just context {symbolTable = SymTable (Map.insert name (length (mem (heap context))) (symTable (symbolTable context)))}

-- | Frees a symbol in the symbol table.
symFree :: Maybe Context -> String -> Maybe Context
symFree Nothing _ = Nothing
symFree (Just context) name = Just context { symbolTable = SymTable (Map.delete name (symTable (symbolTable context))) }

--------------------------------------------------------------------------------
-- LABELS
--------------------------------------------------------------------------------

-- | The labels of the VM. It holds all the created labels that refer to an
-- instruction, and maps them to their address in the instruction pile.
newtype Labels = Labels { labelMap :: Map.Map String Int } deriving (Show)

-- | Creates a new empty label pile.
newLabels :: Labels
newLabels = Labels Map.empty

-- | Sets the address of a label in the label pile.
labelSet :: Maybe Context -> String -> Int -> Maybe Context
labelSet Nothing _ _ = Nothing
labelSet (Just context) name address = Just context { labels = Labels (Map.insert name address (labelMap (labels context))) }

-- | Gets the address of a label in the label pile. If the label isnt
-- allocated, it returns Nothing.
labelGet :: Maybe Context -> String -> Maybe Int
labelGet Nothing _ = Nothing
labelGet (Just context) name = Map.lookup name (labelMap (labels context))

-- | Frees a label in the label pile.
labelFree :: Maybe Context -> String -> Maybe Context
labelFree Nothing _ = Nothing
labelFree (Just context) name = Just context { labels = Labels (Map.delete name (labelMap (labels context))) }

--------------------------------------------------------------------------------
-- Flags
--------------------------------------------------------------------------------

-- | The flags of the VM. It holds all the flags that can be set or unset.
data Flag = ZF | SF | OF | CF | PF | AF
    deriving (Eq, Ord, Show)

newtype Flags = Flags { flagMap :: Map.Map Flag Bool } deriving (Show)

-- | Creates a new empty set of flags.
newFlags :: Flags
newFlags = Flags (Map.fromList [(ZF, False), (SF, False), (OF, False), (CF, False), (PF, False), (AF, False)])

-- | Sets the value of a flag.
flagSet :: Maybe Context -> Flag -> Bool -> Maybe Context
flagSet Nothing _ _ = Nothing
flagSet (Just context) flag value = Just context { flags = Flags (Map.insert flag value (flagMap (flags context))) }

-- | Gets the value of a flag.
flagGet :: Maybe Context -> Flag -> Maybe Bool
flagGet Nothing _ = Nothing
flagGet (Just context) flag = Map.lookup flag (flagMap (flags context))

--------------------------------------------------------------------------------
-- INSTRUCTIONS
--------------------------------------------------------------------------------

data ParamType = Register | Immediate | Memory | Symbol
    deriving (Eq, Ord, Show)

-- | A Param stores its type and its value.
data Param = Param {
    paramType :: ParamType
    , paramValue :: Int
} deriving (Eq, Ord, Show)

data Instruction = Mov Param Param
            | Push Param
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- CONTEXT
--------------------------------------------------------------------------------

-- | The context of the VM.
-- It contains the registries, the stack, the heap, and a pile of instructions.
data Context = Context {
    registers :: Registers
    , stack :: Stack
    , heap :: Heap
    , instructions :: [Instruction]
    , symbolTable :: SymTable
    , labels :: Labels
    , flags :: Flags
    , instructionPointer :: Int
} deriving (Show)

-- | Creates a new empty context.
newContext :: Context
newContext = Context newRegisters newStack newHeap [] newSymTable newLabels newFlags 0

-- | Sets the value of the instruction pointer.
ipSet :: Maybe Context -> Int -> Maybe Context
ipSet Nothing _ = Nothing
ipSet (Just context) value = if value < 0 || value >= length (instructions context) then
        Nothing
    else
        Just context { instructionPointer = value }

-- | Gets the value of the instruction pointer.
ipGet :: Maybe Context -> Maybe Int
ipGet Nothing = Nothing
ipGet (Just context) = Just (instructionPointer context)

-- | Increments the value of the instruction pointer.
ipInc :: Maybe Context -> Maybe Context
ipInc Nothing = Nothing
ipInc (Just context) = if instructionPointer context + 1 >= length (instructions context) then
        Nothing
    else
        Just context { instructionPointer = instructionPointer context + 1 }

-- | Executes the next instruction.
-- TODO CALL THE ACTUAL INSTRUCTION
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
execInstructions :: Maybe Context -> Maybe Context
execInstructions Nothing = Nothing
execInstructions ctx = execInstructions (ipInc c)
    where c = case ctx of
            Nothing -> Nothing
            Just context -> if instructionPointer context + 1 >= length (instructions context) then
                    Nothing
                else
                    evalOneInstruction context (instructions context !! instructionPointer context)

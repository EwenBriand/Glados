module Pointer (

) where

import VM
import Lexer
import VM (Context(Context))

-- -- | @help: allocates a range of memory in the heap, and returns its address
-- arrayAllocAndStore :: Context -> [ASTNode] -> (Int, Maybe Context)
-- -- no empty arrays yet
-- arrayAllocAndStore ctx [] = (0, Nothing)
-- arrayAllocAndStore ctx arr = (ptr, Just c)
--     where
--         (ptr, c) = case heapAllocRange (Just ctx) (length arr) of
--             (_, Nothing) -> (0, ctx) -- returns null!
--             (p, Just c1) -> (p, c1)

-- cpyHaskellArrayToHeap :: Maybe Context -> [ASTNode] -> Int -> Maybe Context
-- cpyHaskellArrayToHeap ctx [] _ = ctx
-- cpyHaskellArrayToHeap Nothing _ _ = Nothing
-- cpyHaskellArrayToHeap ctx [x:xs] addr = cpyHaskellArrayToHeap xs (addr + 1) newc
--     where
--         newc = heapSet

-- -- | The array node is only used to declare the array, else the array will
-- -- be represented by a variable thus bypassing the need for the array node.
-- -- The array node will thus allocate memory for the elements of the array.
-- implASTNode :: Maybe Context -> ASTNode -> Maybe Context
-- implASTNode Nothing _ = Nothing
-- implASTNode (Just ctx) (ASTNodeArray arr) = arrayAllocAndStore ctx arr
-- implASTNode (Just ctx) _ = Nothing

-- | @params:
--    ptr: the pointer to the beginning of the array
--    index: the index of the element to write
--    value: the value to write
-- equivalent to the following ASM instructions:
-- mov [ptr + index * 4], value
hASMArrayWrite :: Int -> Int -> Int -> [Instruction]
hASMArrayWrite ptr index value = [Mov (Mem (ptr + index * 4)) (Imm value)]

-- | pushes the instructions to allocate the space for the array
-- equivalent to :
-- mov eax, 0x2d ; syscall number for sbrk
-- mov ebx, arraySize
-- int 0x80
--
-- mov ebx, eax ; saving the pointer to the allocated memory
--
-- mov esi ebx ; esi will be used to iterate over the array
-- add esi, arraySize * 4 ; upper limit
-- loopStart:
-- (eval one child node)
-- mov [ebx], eax ; storing the value of the child node in the allocated memory
-- add ebx, 4 ; incrementing the pointer to the next element of the array
--
--

hASMArrayAlloc :: Int -> [Instruction]
hASMArrayAlloc size = [
    Mov EAX (Imm 0x2d),
    Mov EBX (Imm size * 4),
    Int 0x80,
]

hASMArrayCopyNodes :: [ASTNode] -> [Instruction]
hASMArrayCopyNodes [] = []


pushHASMArrayInstr :: Context -> [ASTNode] -> MaybeContext
pushHASMArrayInstr ctx [] = Just ctx
pushHASMArrayInstr ctx l = Just ctx {
    instructions = (instructions ctx) ++ (hASMArrayAlloc (length l)) ++
}

ASTNodeArrayToHASM :: Maybe Context -> ASTNode -> MaybeContext
ASTNodeArrayToHASM Nothing _ = Nothing
ASTNodeArrayToHASM (Just ctx) (ASTNodeArray arr) = pushHASMArrayInstr (Just ctx) arr
ASTNodeArrayToHASM (Just ctx) _ = Nothing

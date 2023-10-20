section .data
    ; no data yet
section .text
    global _start

_start:
<<<<<<< HEAD
    mov rax, 1
    jmp _test2
_test:
    mov rax, 3
_test2:
    mov ebx, 1
    mov ecx, 2
    jmp _test
=======
    push 1
    push 42
    push -10
>>>>>>> 6446cf85a687530301ad69587581bf97080446cd

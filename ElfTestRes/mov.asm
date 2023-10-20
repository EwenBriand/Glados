section .data
    ; no data yet
section .text
    global _start

_start:
    mov rax, 1
    jmp _test2
_test:
    mov rax, 3
_test2:
    mov ebx, 1

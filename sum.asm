section .data
    ; no data
section .text
    global _start

_start:
    mov eax, 1
    add eax, 2

    ; call exit
    xor ebx, ebx
    mov ebx, eax
    mov eax, 1
    int 0x80

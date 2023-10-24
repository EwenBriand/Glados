section .data
    ; no data yet
section .text
    global _start
    global call_1
    global call_2

_start:
    enter 0, 0
    call call_1
    call call_2
    xor ebx, ebx
    mov ebx, eax
    mov eax, 1
    int 0x80

call_1:
    enter 0, 0
    mov eax, 1
    leave
    ret

call_2:
    enter 0, 0
    mov eax, 2
    leave
    ret

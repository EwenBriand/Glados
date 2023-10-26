section .data
    ; no data yet
    myVar   dd 42

section .text
    global _start

_start:
    mov dword [myVar], 40
    mov eax, [myVar]

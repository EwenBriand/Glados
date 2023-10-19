section .data
    ; no data yet
section .text
    global _start

_start:
    mov eax, dword [rsp + 42]
    mov ecx, dword [rsp + 42]
    mov edx, dword [rsp + 42]
    mov ebx, dword [rsp + 42]
    mov esp, dword [rsp + 42]
    mov ebp, dword [rsp + 42]
    mov esi, dword [rsp + 42]
    mov edi, dword [rsp + 42]


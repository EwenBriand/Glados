section .data
    ; no data yet
section .text
    global _start

_start:
    xor rax, rax
    mov rax, 1
    push rax
    xor rax, rax
    mov rax, 2
    pop rdi
    add rax, rdi
    mov rdi, 60 ; sys_exit
    xor rax, rax
    syscall

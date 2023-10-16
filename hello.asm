; FILEPATH: /hello.asm

section .data
    hello db 'Hello, world!', 0

section .text
    global _start

_start:
    ; write hello message to stdout
    mov rax, 1          ; syscall for write
    mov rdi, 1          ; stdout file descriptor
    mov rsi, hello      ; message to write
    mov rdx, 13         ; message length
    syscall

    ; exit program with status code 0
    mov rax, 60         ; syscall for exit
    xor rdi, rdi        ; exit status code 0
    syscall

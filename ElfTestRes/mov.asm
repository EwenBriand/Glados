section .text
global _start

_start:
    enter 0,0
    xor eax, eax
    mov eax, 0x2a
    push rdx
    push rcx
    push rbx
    push rax
	mov	edx, 2    ;message length
	mov	ecx, msg42    ;message to write
	mov	ebx, 1	    ;file descriptor (stdout)
	mov	eax, 4	    ;system call number (sys_write)
	int	0x80        ;call kernel
    pop rax
    pop rbx
    pop rcx
    pop rdx
    xor ebx, ebx
    mov ebx, eax
	mov	eax, 1	    ;system call number (sys_exit)
	int	0x80        ;call kernel

msg42	db	'42'	;our dear string


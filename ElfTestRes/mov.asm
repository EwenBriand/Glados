section .text
global _start

_start:
    push rdx
    push rcx
    push rbx
    push rax
	mov	edx, 2    ;message length
	mov	ecx, 0x2a0    ;message to write
	mov	ebx, 1	    ;file descriptor (stdout)
	mov	eax, 4	    ;system call number (sys_write)
	int	0x80        ;call kernel
    pop rax
    pop rbx
    pop rcx
    pop rdx
    push rdx
    push rcx
    push rbx
    push rax
	mov	edx, 2    ;message length
	mov	ecx, 0x40000100    ;message to write
	mov	ebx, 1	    ;file descriptor (stdout)
	mov	eax, 4	    ;system call number (sys_write)
	int	0x80        ;call kernel
    pop rax
    pop rbx
    pop rcx
    pop rdx

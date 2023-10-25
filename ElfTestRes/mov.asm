section .data
    ; no data yet

section .text
;    0:	8b 45 08             	mov    0x8(%rbp),%eax
;    3:	8b 4d 08             	mov    0x8(%rbp),%ecx
;    6:	8b 55 08             	mov    0x8(%rbp),%edx
;    9:	8b 5d 08             	mov    0x8(%rbp),%ebx
;    c:	8b 65 08             	mov    0x8(%rbp),%esp
;    f:	8b 6d 08             	mov    0x8(%rbp),%ebp
;   12:	8b 75 08             	mov    0x8(%rbp),%esi
;   15:	8b 7d 08             	mov    0x8(%rbp),%edi
    mov eax, [rbp - 16]
    mov ecx, [rbp - 16]
    mov edx, [rbp - 16]
    mov ebx, [rbp - 16]
    mov esp, [rbp - 16]
    mov ebp, [rbp - 16]
    mov esi, [rbp - 16]
    mov edi, [rbp - 16]

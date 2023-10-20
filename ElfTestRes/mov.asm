section .data
    ; no data yet
section .text
    global _start

_start:
    xor eax, eax
_labelFoo:
    xor eax, edi
_labelBar:
    xor eax, esi

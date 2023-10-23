section .data
    ; no data yet
section .text
    global _start

_start:
    je  _test
    jne _test
    js _test
    jns _test
    jg _test
    jge _test
    jl _test
    jle _test
    ja _test
    jae _test
    jb _test
    jbe _test
_test:
    je  _start
    jne _start
    js _start
    jns _start
    jg _start
    jge _start
    jl _start
    jle _start
    ja _start
    jae _start
    jb _start
    jbe _start


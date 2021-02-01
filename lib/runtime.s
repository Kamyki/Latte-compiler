
section .bss
var1: resq 1

section .text
global printInt
global printString
global error
global readInt
global readString
global _concatString
global _cmpString
global _alloc_size

extern printf, scanf, getchar, realloc, malloc, strlen, memcpy, fflush, strcmp

; alloc 1st_arg * 8bytes bytes for structure
_alloc_size:
push rbp
mov rbp, rsp
and rsp, -16

mov rdi, [rbp + 16]
imul rdi, 8
xor rax, rax
call malloc
test rax, rax
je .not_malloc

mov rsp, rbp
pop rbp
ret

.not_malloc:
lea rdi, [rel __alloc_error]
push rdi
call printString


printInt:
push rbp
mov rbp, rsp
and rsp, -16

mov rsi, [rbp + 16]
lea rdi, [rel __int_format]
xor rax, rax
call printf
xor rax, rax
call fflush

mov rsp, rbp
pop rbp
ret

printString:
push rbp
mov rbp, rsp
and rsp, -16

mov rsi, [rbp + 16]
lea rdi, [rel __string_format]
xor rax, rax
call printf
xor rax, rax
call fflush

mov rsp, rbp
pop rbp
ret

error:
push rbp
mov rbp, rsp
and rsp, -16

lea rdi, [rel __runtime_error]
xor rax, rax
call printf
xor rax, rax
call fflush

mov rdi, 1
mov rax, 60
syscall

mov rsp, rbp
pop rbp
ret

readInt:
push rbp
mov rbp, rsp
and rsp, -16

lea rdi, [rel __int_format]
lea rsi, [rel var1]

call scanf
mov rax, [rel var1]

mov rsp, rbp
pop rbp
ret

readString:
push rbp
mov rbp, rsp
and rsp, -16

mov rdi, 0
mov rsi, 32
call realloc
mov r12, rax
mov r13, 0
mov r14, 32

.loop:
cmp r13, r14
je .bigger
call getchar
cmp rax, 0
je .final
cmp rax, 0x0a
je .final
mov [r12 + r13], rax
inc r13
jmp .loop

.final:
mov byte [r12 + r13], 0
mov rdi, r12
mov rsi, r13
add rsi, 1
call realloc

mov rsp, rbp
pop rbp
ret

.bigger:
mov rdi, r12
shl r14, 1
mov rsi, r14
call realloc
mov r12, rax
jmp .loop


_concatString:
push rbp
mov rbp, rsp
and rsp, -16

mov r9, [rbp + 16]
mov r10, [rbp + 24]

mov rdi, [rbp + 16]
call strlen
mov r12d, eax ; len 1

mov rdi, [rbp + 24]
call strlen
mov r13d, eax ; len 2

mov r14d, r12d
add r14d, r13d
add r14d, 1 ; len(1+2) + 1
mov eax, r14d
cdqe

mov rdi, rax
xor rax, rax
call malloc
test rax, rax
je .not_malloc
mov r15, rax ; addr

mov rdi, rax
mov rsi, [rbp + 16]
mov edx, r12d
call memcpy

mov rdi, r15; addr
add rdi, r12; addr + len1
mov rsi, [rbp + 24]
mov edx, r13d
call memcpy

add rax, r14
sub rax, 1
mov byte [rax], 0

mov rax, r15
mov rsp, rbp
pop rbp
ret

.not_malloc:
lea rdi, [rel __alloc_error]
push rdi
call printString

mov rdi, 1
mov rax, 60
syscall

_cmpString:
push rbp
mov rbp, rsp
and rsp, -16

mov rdi, [rbp + 16]
mov rsi, [rbp + 24]
call strcmp
test rax, rax
setz al

mov rsp, rbp
pop rbp
ret

section .data
__alloc_error db 'Memory allocation error',0x0a,0
__int_format db '%ld',0x0a,0
__string_format db '%s',0x0a,0
__runtime_error db 'runtime error',0x0a,0

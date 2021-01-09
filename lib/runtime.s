
section .text
global printInt
global printString
global errpr
global readInt
global readString
global _concatString

extern printf, scanf, exit, getchar, realloc, strlen, memcpy, fflush



printInt:
push rbp
mov rbp, rsp
and rsp, -16

mov rsi, [rbp + 16]
lea rdi, [rel __int_format]
mov rax, 0

call printf
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
call printf
call fflush

mov rsp, rbp
pop rbp
ret

error:
push rbp
mov rbp, rsp
and rsp, -16

mov rdi, 1
call exit

mov rsp, rbp
pop rbp
ret

readInt:
push rbp
mov rsi, rsp
mov rdi, [rel __int_format]
sub rsp, 8

mov rbp, rsp
and rsp, -16

call scanf
mov rsp, rbp

add rsp, 8
mov rax, [rsp-8]

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

loop:
cmp r13, r14
je bigger
call getchar
mov [r12 + r13], rax
inc r13
cmp rax, 0
je final
jmp loop

final:
mov rdi, r12
mov rsi, r13
call realloc

mov rsp, rbp
pop rbp
ret

bigger:
mov rdi, r12
shl r14, 1
mov rsi, r14
call realloc
mov r12, rax
jmp loop


_concatString:
push rbp
mov rbp, rsp
and rsp, -16

mov r9, [rbp + 16]
mov r10, [rbp + 24]

mov rdi, [rbp + 16]
call strlen
mov r12, rax ; len 1

mov rdi, [rbp + 24]
call strlen
mov r13, rax ; len 2

mov r14, r12
add r14, r13
add r14, 1 ; len(1+2) + 1

mov rdi, 0
mov rsi, r14
call realloc
mov r15, rax ; addr

mov rdi, rax
mov rsi, [rbp + 16]
mov rdx, r12
call memcpy

mov rdi, r15
add rdi, r12
mov rsi, [rbp + 24]
mov rdx, r13
call memcpy

add rax, r14
mov byte [rax], 0

mov rax, r15
mov rsp, rbp
pop rbp
ret

__int_format db '%ld',0x0a,0
__string_format db '%s',0x0a,0

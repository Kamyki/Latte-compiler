
section .text
global printInt
global printString
global errpr
global readInt
global readString
global concatString

extern printf, scanf, exit, getchar, realloc, strlen, memcpy



printInt:
lea rsi, [rsp + 16]
lea rdi, [rel __int_format]
mov rax, 0

push rbp
mov rbp, rsp
and rsp, -16
call printf
mov rsp, rbp
pop rbp
ret

printString:
push rbp
mov rbp, rsp
and rsp, -16

mov rdi, [rbp + 24]
call printf

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
mov r10, rax
mov r9, 0
mov r8, 32

loop:
cmp r9, r8
je bigger
call getchar
mov [r10 + r9], rax
inc r9
cmp rax, 0
je final
jmp loop

final:
mov rdi, r10,
mov rsi, r9
call realloc

mov rsp, rbp
pop rbp
ret

bigger:
mov rdi, r10
shl r8, 1
mov rsi, r8
call realloc
mov r10, rax
jmp loop


concatString:
push rbp
mov rbp, rsp
and rsp, -16

mov r9, [rbp + 24]
mov r10, [rbp + 32]

mov rdi, r9
call strlen
mov r11, rax

mov rdi, r10
call strlen
mov r12, rax

mov r13, r11
add r13, r12
add r13, 1

mov rdi, 0
mov rsi, r13
call realloc
mov r14, rax

mov rdi, rax
mov rsi, r9
mov rdx, r11
call memcpy

mov rdi, r14
add rdi, r11
mov rsi, r10
mov rdx, r12
call memcpy

mov rax, r13
mov byte [rax], 0

mov rax, r14
mov rsp, rbp
pop rbp
ret

__int_format db '%d',0x0a,0

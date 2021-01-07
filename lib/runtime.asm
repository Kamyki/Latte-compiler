
section .txt
global printInt, printString, errpr, readInt, readString
extern printf



printInt:
push rsi
push rdi
push rax
mov rsi, [esp + 8]
mov rdi, [rel __int_format]
mov rax, 0
call printf
pop rax
pop rdi
pop rsi
exit

printString:
push rdi
push rax
mov rdi, [esp + 8]
call printf
pop rax
pop rdi

error:


readInt:

readString:


__int_format db '%d',0x0a,0
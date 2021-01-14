Compiler for Latte language (https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/Latte/)
written in Rust language.

Installation:
```
make
```
It will download Rust compiler with developer tools locally to ./cargo and ./rustup folders and 
compile programs using them. It will edit env variables for this shell session.

#### Usage:
```
latc_x86_64 <input_file>
```

#### Dependencies:

- BNFC does not produce parsers for Rust backends so instead lalrpop (https://github.com/lalrpop/lalrpop) has been used.
- For pretty-print of frontend errors codespan-reporting crate has been used (https://github.com/brendanzab/codespan)
- For compiling asm code NASM compiler
- For linking GNU linker (ld)

#### Calling convention:

None of the registers are callee saved, so before each call their values are saved in memory. RBP is used to keep frame address.
Arguments are passed using stack in reverse order.
Returned value is kept in RAX register.

#### What is inside:
- Compiler of basic Latte on x86_64 architecture.
- Register allocation with liveliness analysis. By default it produces operations in form `op Register [Imm|R|Memory]` in this preference order. It alleviates register pressure,
but you can force `op Register Register` by looking for comments with `USE_REGISTERS` in `backend/assebler_transformer.rs`. Compilation flag TBD. 
- Boolean values made with lazy evaluation.
- Jumps to next line are removed, but labels are left alone. Order of labeled blocks is semi-random so sometimes compiler produces additional jumps.
- Static string built into binary. Runtime string allocated using c malloc.
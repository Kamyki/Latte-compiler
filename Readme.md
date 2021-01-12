Compiler for Latte language (https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/Latte/)
written in Rust language.

Installation:
```
make
```
It will download Rust compiler with developer tools locally to ./cargo and ./rustup folders and 
compile programs using them. It will edit env variables for this shell session.

Usage:
```
latc_x86_64 <input_file>
```

Dependencies:

- BNFC does not produce parsers for Rust backends so instead lalrpop (https://github.com/lalrpop/lalrpop) has been used.
- For pretty-print of frontend errors codespan-reporting crate has been used (https://github.com/brendanzab/codespan)
- For compiling asm code NASM compiler
- For linking GNU linker (ld)

What is inside:
- Compiler of basic Latte on x86_64 architecture with register allocation (without liveliness analysis).
- Boolean values made with lazy evaluation 
- Jumps to next line are removed, but labels are left alone. Order of labeled blocks is semi-random so sometimes compiler produces additional jumps.
- Static string built into binary. Runtime string allocated using c malloc.
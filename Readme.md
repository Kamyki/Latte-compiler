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
latc <input_file>
```

Dependencies:

BNFC does not produce parsers for Rust backends so instead lalrpop (https://github.com/lalrpop/lalrpop) has been used.
For pretty-print of frontend errors codespan-reporting crate has been used (https://github.com/brendanzab/codespan)
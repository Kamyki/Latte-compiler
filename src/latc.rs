#[macro_use]
extern crate serde_derive;

use std::{env, io, path};
use std::fs::File;
use std::io::{Read, Write};
use std::process::Command;
use std::process::exit;

use docopt::Docopt;

use latte::backend::transform;
use latte::error_handling::{map_file, print_errors};
use latte::frontend::check_semantics;
use latte::parser;

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.argv(env::args()).deserialize())
        .unwrap_or_else(|e| e.exit());

    let path = path::Path::new(&args.arg_input);

    let mut s = String::new();

    if let Err(err) = File::open(path).and_then(|mut f| f.read_to_string(&mut s)) {
        println!("Input `{}`: I/O Error {}", path.to_str().unwrap(), err);
        return;
    }
    let loc_map = map_file(&args.arg_input, &s);


    let result = parser::parse(&loc_map)
        .and_then(|mut program| check_semantics( &mut program)
            .and_then(|maps| transform(maps, &mut program)));


    match result {
        Ok((graph, code)) => {
            eprintln!("OK");
            println!("functions: {:?}", graph.functions);
            println!("open_blocks: {:?}", graph.current_block);

            for (l, b) in graph.iter() {
                println!("{:?}", l);
                println!("jumps: {:?}", b.jumps);
                println!("{:?}", b.code);
            }
            println!();
            for c in code.iter() {
                println!("{}", c);
            }

            let asm_path = path.with_extension("s");
            if let Err(err) = File::create(&asm_path)
                .and_then(|mut f| {
                    code.iter().for_each(|l| writeln!(f, "{}", l).unwrap());
                    Ok(())
                }) {
                println!("Error writing output to {}: Error {}", asm_path.to_str().unwrap(), err);
                exit(1);
            }

            let o= Command::new("sh")
                .args(&["-c", &format!("nasm -f elf64 -F dwarf -g  {}", asm_path.to_str().unwrap())])
                .output()
                .expect("Cannot run nasm to produce .o file");

            if !o.status.success() {
                io::stderr().write_all(&o.stderr).unwrap();
                exit(1);
            }

            let o = Command::new("sh")
                .args(&["-c", &format!("ld {} ./lib/runtime.o -o {} -lc --dynamic-linker=/lib64/ld-linux-x86-64.so.2", asm_path.with_extension("o").to_str().unwrap(), asm_path.with_extension("").to_str().unwrap())])
                .output()
                .expect("Cannot run ld to produce executalbe file");



            if !o.status.success() {
                io::stderr().write_all(&o.stderr).unwrap();
                exit(1);
            }

            exit(0)
        }
        Err(err) => {
            eprintln!("ERROR");
            print_errors(&loc_map, err.as_slice());
            exit(1)
        }
    }
}

const USAGE: &'static str = "
Usage: latc_llvm <input>
Parses file input and compiles it.
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_input: String,
}
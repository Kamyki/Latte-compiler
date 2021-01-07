use latte::error_handling::{print_errors, map_file};
use std::fs::File;
use std::{path, env};
use std::io::Read;
use docopt::Docopt;
use latte::frontend::check_semantics;
use latte::parser;
use latte::backend::transform;
use std::process::exit;

#[macro_use]
extern crate serde_derive;

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
            for c in code {
                println!("{:?}", c);
            }
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
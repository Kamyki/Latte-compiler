use latte::error_handling::{print_errors, map_file};
use std::fs::File;
use std::{path, env};
use std::io::Read;
use docopt::Docopt;
use latte::frontend::check_semantics;
use latte::parser;

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


    match parser::parse(&loc_map) {
        Ok(mut program) => {
            println!("{:?}", program);

            check_semantics(&loc_map, &mut program);
            println!("{}", loc_map.source())
        }
        Err(err) => print_errors(&loc_map, err.as_slice())
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
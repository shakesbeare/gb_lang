mod error;
mod lexer;

use lexer::run_lexer;
use std::fs;
fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        println!("Invalid number of arguments");
        std::process::exit(1);
    }

    let filename = &args[1];
    let file = fs::read_to_string(filename).unwrap();

    let tokens = run_lexer(file);

    match tokens {
        Ok(val) => println!("{:?}", val),
        Err(val) => {
            println!("{:?}", val);
            std::process::exit(2);
        }
    }
}

extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod gb_type;
mod interpreter;
mod parser;
mod scope;

use clap::Parser as ClapParser;
use pest::Parser;
use std::io;
use std::io::BufRead;

#[derive(clap::Parser)]
struct Cli {
    #[arg(short = 'i', long = "interactive")]
    interactive_mode: bool,

    path: Option<std::path::PathBuf>,
}

fn main() -> io::Result<()> {
    let args = Cli::parse();

    let mut global_scope = interpreter::init();

    match &args.interactive_mode {
        true => {
            if args.path.is_some() {
                static_mode(args.path.unwrap(), &mut global_scope);
            }
            interactive_mode(&mut global_scope)
        }
        false => {
            if !args.path.is_some() {
                eprintln!("No file provided");
            } else {
                static_mode(args.path.unwrap(), &mut global_scope);
            }
            Ok(())
        }
    }
}

fn interactive_mode(global_scope: &mut scope::Scope) -> io::Result<()> {
    for line in io::stdin().lock().lines() {
        match parser::GbParser::parse(parser::Rule::file, &line?) {
            Ok(pairs) => {
                let ast = parser::parse_expr(pairs);
                // println!("{:?}", ast);
                println!("{}", interpreter::evaluate(ast, global_scope));
            }
            Err(e) => {
                eprintln!("{:?}", e);
            }
        }
    }

    Ok(())
}

fn static_mode(path: std::path::PathBuf, global_scope: &mut scope::Scope) {
    let content = std::fs::read_to_string(&path).expect("File not found!");

    match parser::GbParser::parse(parser::Rule::file, &content) {
        Ok(pairs) => {
            let ast = parser::parse_expr(pairs);
            // println!("{:?}", ast);
            interpreter::evaluate(ast, global_scope);
        }
        Err(e) => {
            eprintln!("{:?}", e);
        }
    }
}

mod interpreter;
mod parser;
extern crate pest;

#[allow(unused_imports)]
#[macro_use]
extern crate pest_derive;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        println!("Invalid number of arguments");
        std::process::exit(1);
    }

    let filename = &args[1];

    let file = parser::parse(filename);

    match file {
        #[allow(unused_variables)]
        Ok(ast) => {
            interpreter::evaluate(ast);
        }
        Err(err) => println!("{}", err),
    }
}

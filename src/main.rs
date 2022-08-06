mod error;
mod lexer;

use lexer::run_lexer;

fn main() {
    let tokens = run_lexer("let a: float = 7.23;".to_string());

    match tokens {
        Ok(val) => println!("{:?}", val),
        Err(val) => {
            println!("{:?}", val);
            std::process::exit(1);
        }
    }
}

#[macro_use]
extern crate lazy_static;

mod ast;
mod lexer;
mod parser;
mod token;

use anyhow::Result;

fn main() -> Result<()> {
    let lexer = lexer::Lexer::new("file.gb")?;
    let mut parser = parser::Parser::new(lexer, true);


    let ast = parser.parse_program();
    // parser.lexer.lex_all();
    dbg!(parser.lexer.token_stream);
    dbg!(parser.lexer.lexeme_stream);
    dbg!(ast);

    Ok(())
}

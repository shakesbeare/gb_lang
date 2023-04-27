#[macro_use]
extern crate lazy_static;

mod ast;
mod lexer;
mod parser;
mod token;

use anyhow::Result;

fn main() -> Result<()> {
    let mut lexer = lexer::Lexer::new("file.gb")?;

    // lexer.lex_all()?;
    let mut parser = parser::Parser::new(lexer, false);

    // dbg!(parser.lexer.token_stream);
    // dbg!(parser.lexer.lexeme_stream);

    let ast = parser.parse_program();

    dbg!(ast);

    Ok(())
}

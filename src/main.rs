#[macro_use]
extern crate lazy_static;

mod token;
mod ast;
mod lexer;
mod parser;

use anyhow::Result;

fn main() -> Result<()> {
    let mut lexer = lexer::Lexer::new("file.gb")?;

    lexer.lex_all()?;

    dbg!(lexer.token_stream);
    dbg!(lexer.lexeme_stream);

    Ok(())
}


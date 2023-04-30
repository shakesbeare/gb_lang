mod ast;
mod lexer;
mod parser;
mod token;
mod tests;

use anyhow::Result;

fn main() -> Result<()> {
    let lexer = lexer::Lexer::open_file("file.gb")?;
    let mut parser = parser::Parser::new(lexer, true);

    let ast = parser.parse();
    // parser.lexer.lex_all();
    dbg!(parser.lexer.token_stream);
    dbg!(parser.lexer.lexeme_stream);
    dbg!(ast);

    Ok(())
}


use anyhow::Result;
use gb_lang::*;

fn main() -> Result<()> {
    let input = "hello {};\n let x = 7;";
    let mut p = parser::Parser::new(
        lexer::Lexer::from(input.as_bytes()),
        Box::new(parser::error::DefaultErrorHandler { input }),
        false,
    );
    p.parse().unwrap();
    Ok(())
}

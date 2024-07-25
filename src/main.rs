use anyhow::Result;
use gb_lang::*;

fn main() -> Result<()> {
    let input = "let x = fn {}
let x = 7;";
    let mut p = parser::Parser::new(
        lexer::Lexer::from(input.as_bytes()),
        Box::new(parser::error::DefaultErrorHandler { input }),
        false,
    );
    let ast = p.parse().unwrap();
    dbg!(ast);
    Ok(())
}

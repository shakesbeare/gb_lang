mod ast;
mod lexer;
mod parser;
mod tests;
mod token;
mod interpreter;
mod gb_type;
mod scope;

use anyhow::Result;

fn main() -> Result<()> {
    // let lexer = lexer::Lexer::open_file("file.gb")?;
    // let mut parser = parser::Parser::new(lexer, true);
    //
    // let ast = parser.parse();
    // parser.lexer.lex_all();
    // dbg!(parser.lexer.token_stream);
    // dbg!(parser.lexer.lexeme_stream);
    // dbg!(parser.lexer.point_stream);
    // dbg!(ast);
    
    let mut interpreter = interpreter::Interpreter::open_file("file.gb");
    let res = interpreter.interpret_inner();
    
    dbg!(res);

    Ok(())
}

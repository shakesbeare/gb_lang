use anyhow::Result;
use gb_lang::*;

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
    //
    // let res = interpreter.interpret_inner();
    //
    // dbg!(res);

    // let x = Node::new(
    //     NodeKind::Program,
    //     NodeChildren::Many(vec![Node::new(
    //         NodeKind::Program,
    //         NodeChildren::Many(vec![]),
    //     )]),
    // );
    //
    // println!("{:#?}", x);
    let input = "hello {";
    let mut p = parser::Parser::new(lexer::Lexer::from(input.as_bytes()), error::ErrorHandler { input }, false);
    p.parse();
    let input = "{";
    let mut p = parser::Parser::new(lexer::Lexer::from(input.as_bytes()), error::ErrorHandler { input }, false);
    p.parse();
    Ok(())
}

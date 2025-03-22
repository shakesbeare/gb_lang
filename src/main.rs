use gbc_lex::GbLexer;

fn main() {
    let input = r#"
    word 1234 0xff 0b10101 fn true false 
    return enum struct continue break while
    for & ! + [ { ( ) } ] * !
        "#;

    for tok in input.lexer() {
        let _ = tok.map(|t| {
            print!("{:?} ", t.kind);
        });
    }
    println!();
}

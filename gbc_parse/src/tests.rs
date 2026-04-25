use crate::gbc_parse::parse_tree::AstNode;
use crate::GbParser;

#[test]
fn parse_empty() {
    let input = "";
    let mut parser = input.gb_parser();
    let output = parser.parse();
    assert!(output.stmts.is_empty());
}

#[test]
fn parse_ident_expr() {
    let input = "word";
    let mut parser = input.gb_parser();
    let output = parser.parse();
    assert!(output.stmts.len() == 1);
    let stmt = output.stmts.first().unwrap();
    let expr = stmt.kind.unwrap_expr_ref();
    let ident = expr.kind.unwrap_ident_ref();
    assert_eq!(ident.render(input), input);
}

#[test]
fn parse_expr_term() {
    let input = "word;";
    let mut parser = input.gb_parser();
    let output = parser.parse();
    assert!(output.stmts.len() == 1);
    let stmt = output.stmts.first().unwrap();
    let expr = stmt.kind.unwrap_expr_term_ref();
    let ident = expr.kind.unwrap_ident_ref();
    assert_eq!(ident.render(input), "word");
    assert_eq!(stmt.render(input), input);
}

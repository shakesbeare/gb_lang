use lexer::Lexer;

use crate::GbLexer as _;

use super::*;

fn simple_lex_test(input: &str, expected: TokenKind) {
    let tok = Lexer::new(input).next().unwrap().unwrap();
    assert_eq!(tok.literal, input);
    assert_eq!(tok.kind, expected);
    assert_eq!(tok.location, Location { line: 0, col: 0 })
}

#[test]
fn lex_int_literal() {
    simple_lex_test("12345", TokenKind::DecimalLiteral);
}

#[test]
fn lex_float_literal() {
    simple_lex_test("12.345", TokenKind::DecimalLiteral);
}

#[test]
fn lex_numeral_separators() {
    simple_lex_test("12_345", TokenKind::DecimalLiteral);
}

#[test]
fn lex_hexadecimal_literal() {
    simple_lex_test("0xff", TokenKind::HexadecimalLiteral);
}

#[test]
#[should_panic]
fn lex_invalid_hex() {
    simple_lex_test("0x", TokenKind::HexadecimalLiteral);
}

#[test]
fn lex_identifier() {
    simple_lex_test("_word123", TokenKind::Identifier);
}

#[test]
fn lex_string_literal() {
    simple_lex_test(
        "\"Hello, World! #1234567890+[{(&=)}]*\"",
        TokenKind::StringLiteral,
    );
}

#[test]
fn lex_comment() {
    simple_lex_test("// this is a comment", TokenKind::Comment);
}

#[test]
fn lex_block_comment() {
    simple_lex_test(
        "/* this is a block
            comment */",
        TokenKind::Comment,
    );
}

#[test]
fn skip_whitespace() {
    let input = "    word     \n\t\r1234";
    let mut l = Lexer::new(input);
    let first = l.next().unwrap().unwrap();
    let second = l.next().unwrap().unwrap();
    assert_eq!(first.kind, TokenKind::Identifier);
    assert_eq!(second.kind, TokenKind::DecimalLiteral);
}

#[test]
fn lex_l_paren() {
    simple_lex_test("(", TokenKind::LParen);
}
#[test]
fn lex_r_paren() {
    simple_lex_test(")", TokenKind::RParen);
}

#[test]
fn lex_l_brace() {
    simple_lex_test("{", TokenKind::LBrace);
}

#[test]
fn lex_r_brace() {
    simple_lex_test("}", TokenKind::RBrace);
}

#[test]
fn lex_l_bracket() {
    simple_lex_test("[", TokenKind::LBracket);
}

#[test]
fn lex_r_bracket() {
    simple_lex_test("]", TokenKind::RBracket);
}

#[test]
fn lex_true() {
    simple_lex_test("true", TokenKind::True);
}

#[test]
fn lex_false() {
    simple_lex_test("false", TokenKind::False);
}

#[test]
fn lex_return() {
    simple_lex_test("return", TokenKind::Return);
}

#[test]
fn lex_fn() {
    simple_lex_test("fn", TokenKind::Fn);
}

#[test]
fn lex_while() {
    simple_lex_test("while", TokenKind::While);
}

#[test]
fn lex_for() {
    simple_lex_test("for", TokenKind::For);
}

#[test]
fn lex_continue() {
    simple_lex_test("continue", TokenKind::Continue);
}

#[test]
fn lex_break() {
    simple_lex_test("break", TokenKind::Break);
}

#[test]
fn lex_struct() {
    simple_lex_test("struct", TokenKind::Struct);
}

#[test]
fn lex_enum() {
    simple_lex_test("enum", TokenKind::Enum);
}

#[test]
fn lex_assign() {
    simple_lex_test("=", TokenKind::Assign);
}

#[test]
fn lex_add() {
    simple_lex_test("+", TokenKind::Add);
}

#[test]
fn lex_subtract() {
    simple_lex_test("-", TokenKind::Subtract);
}

#[test]
fn lex_multiply() {
    simple_lex_test("*", TokenKind::Multiply);
}

#[test]
fn lex_divide() {
    simple_lex_test("/", TokenKind::Divide);
}

#[test]
fn lex_greater_than() {
    simple_lex_test(">", TokenKind::GreaterThan);
}

#[test]
fn lex_less_than() {
    simple_lex_test("<", TokenKind::LessThan);
}

#[test]
fn lex_greater_equals() {
    simple_lex_test(">=", TokenKind::GreaterEquals);
}

#[test]
fn lex_less_equals() {
    simple_lex_test("<=", TokenKind::LessEquals);
}

#[test]
fn lex_equal() {
    simple_lex_test("==", TokenKind::Equal);
}

#[test]
fn lex_not_equal() {
    simple_lex_test("!=", TokenKind::NotEqual);
}

#[test]
fn lex_not() {
    simple_lex_test("!", TokenKind::Not);
}

#[test]
fn lex_ref() {
    simple_lex_test("&", TokenKind::Ref);
}

#[test]
fn lex_deref() {
    simple_lex_test("^", TokenKind::Deref);
}

#[test]
/// Any time the iterator behaves incorrectly, the failing case should be added to this test.
/// It should always have any fallible lexer function The iterator is expected to always return
/// None after returning None once.
///
/// This test technically only guarantees that the first call after a None is also a None, but
/// that should be sufficient.
fn lex_iterator_behaves_correctly() {
    let input = "0x \"";
    let mut iter = input.lexer();
    for _ in iter.by_ref() {}
    assert_eq!(iter.next(), None);
}

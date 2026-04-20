use lexer::Lexer;

use crate::GbLexer as _;

use super::*;

fn simple_lex_test(input: &str, expected: TokenType) {
    let tok = Lexer::new(input).next().unwrap().unwrap();
    assert_eq!(tok.literal, input);
    assert_eq!(tok.ty, expected);
    assert_eq!(
        tok.location,
        Location {
            offset: 0,
            line: 0,
            col: 0
        }
    )
}

#[test]
fn lex_int_literal() {
    simple_lex_test("12345", TokenType::NumericLiteral);
}

#[test]
fn lex_binary_literal() {
    simple_lex_test("0b101", TokenType::NumericLiteral);
}

#[test]
fn lex_float_literal() {
    simple_lex_test("12.345", TokenType::NumericLiteral);
}

#[test]
fn lex_numeral_separators() {
    simple_lex_test("12_345", TokenType::NumericLiteral);
}

#[test]
fn lex_hexadecimal_literal() {
    simple_lex_test("0xff", TokenType::NumericLiteral);
}

#[test]
fn lex_identifier() {
    simple_lex_test("_word123", TokenType::Identifier);
}

#[test]
fn lex_string_literal() {
    simple_lex_test(
        "\"Hello, World! #1234567890+[{(&=)}]*\"",
        TokenType::StringLiteral,
    );
}

#[test]
fn lex_comment() {
    simple_lex_test("// this is a comment", TokenType::Comment);
}

#[test]
fn lex_block_comment() {
    simple_lex_test(
        "/* this is a block
            comment */",
        TokenType::Comment,
    );
}

#[test]
fn lex_l_paren() {
    simple_lex_test("(", TokenType::LParen);
}
#[test]
fn lex_r_paren() {
    simple_lex_test(")", TokenType::RParen);
}

#[test]
fn lex_l_brace() {
    simple_lex_test("{", TokenType::LBrace);
}

#[test]
fn lex_r_brace() {
    simple_lex_test("}", TokenType::RBrace);
}

#[test]
fn lex_l_bracket() {
    simple_lex_test("[", TokenType::LBracket);
}

#[test]
fn lex_r_bracket() {
    simple_lex_test("]", TokenType::RBracket);
}

#[test]
fn lex_true() {
    simple_lex_test("true", TokenType::True);
}

#[test]
fn lex_false() {
    simple_lex_test("false", TokenType::False);
}

#[test]
fn lex_return() {
    simple_lex_test("return", TokenType::Return);
}

#[test]
fn lex_fn() {
    simple_lex_test("fn", TokenType::Fn);
}

#[test]
fn lex_while() {
    simple_lex_test("while", TokenType::While);
}

#[test]
fn lex_for() {
    simple_lex_test("for", TokenType::For);
}

#[test]
fn lex_continue() {
    simple_lex_test("continue", TokenType::Continue);
}

#[test]
fn lex_break() {
    simple_lex_test("break", TokenType::Break);
}

#[test]
fn lex_struct() {
    simple_lex_test("struct", TokenType::Struct);
}

#[test]
fn lex_enum() {
    simple_lex_test("enum", TokenType::Enum);
}

#[test]
fn lex_assign() {
    simple_lex_test("=", TokenType::Equal);
}

#[test]
fn lex_add() {
    simple_lex_test("+", TokenType::Plus);
}

#[test]
fn lex_subtract() {
    simple_lex_test("-", TokenType::Minus);
}

#[test]
fn lex_multiply() {
    simple_lex_test("*", TokenType::Star);
}

#[test]
fn lex_divide() {
    simple_lex_test("/", TokenType::ForwardSlash);
}

#[test]
fn lex_greater_than() {
    simple_lex_test(">", TokenType::GreaterThan);
}

#[test]
fn lex_less_than() {
    simple_lex_test("<", TokenType::LessThan);
}

#[test]
fn lex_bitwise_and() {
    simple_lex_test("&", TokenType::And);
}

#[test]
fn lex_bitwise_or() {
    simple_lex_test("|", TokenType::Pipe);
}

#[test]
fn lex_bitwise_xor() {
    simple_lex_test("^", TokenType::Carat);
}

#[test]
fn lex_not() {
    simple_lex_test("!", TokenType::Bang);
}

#[test]
fn lex_double_equal() {
    simple_lex_test("==", TokenType::EqualEqual);
}

#[test]
fn lex_double_and() {
    simple_lex_test("&&", TokenType::AndAnd);
}

#[test]
fn lex_less_less() {
    simple_lex_test("<<", TokenType::LessLess);
}

#[test]
fn lex_greater_greater() {
    simple_lex_test(">>", TokenType::GreaterGreater);
}

#[test]
fn lex_plus_equal() {
    simple_lex_test("+=", TokenType::PlusEqual);
}

#[test]
fn lex_minus_equal() {
    simple_lex_test("-=", TokenType::MinusEqual);
}

#[test]
fn lex_star_equal() {
    simple_lex_test("*=", TokenType::StarEqual);
}

#[test]
fn lex_forward_slash_equal() {
    simple_lex_test("/=", TokenType::ForwardSlashEqual);
}

#[test]
fn lex_star_star() {
    simple_lex_test("**", TokenType::StarStar);
}

#[test]
fn lex_pipe_pipe() {
    simple_lex_test("||", TokenType::PipePipe);
}

#[test]
fn lex_back_slash() {
    simple_lex_test("\\", TokenType::BackSlash);
}

#[test]
fn skip_whitespace() {
    let input = "    word     \n\t\r1234";
    let mut l = Lexer::new(input);
    let first = l.next().unwrap().unwrap();
    let second = l.next().unwrap().unwrap();
    assert_eq!(first.ty, TokenType::Identifier);
    assert_eq!(second.ty, TokenType::NumericLiteral);
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
    let mut iter = input.gb_lexer();
    for _ in iter.by_ref() {}
    assert_eq!(iter.next(), None);
}

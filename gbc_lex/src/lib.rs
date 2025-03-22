mod lexer;
mod position_chars;

// 1) take in any kind of string to process
// 2) define patterns with regex-like syntax
// 3) output a sequence of tokens

use position_chars::PositionChars;

#[derive(Debug, Clone, thiserror::Error)]
#[error("{msg} at {location}")]
pub struct SyntaxError {
    location: Location,
    msg: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    line: usize,
    col: usize,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, col {}", self.line, self.col)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    DecimalLiteral,
    HexadecimalLiteral,
    StringLiteral,
    Identifier,
    Comment,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    True,
    False,
    Return,
    Fn,
    While,
    For,
    Continue,
    Break,
    Struct,
    Enum,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token<'a> {
    literal: &'a str,
    kind: TokenKind,
    location: Location,
}

pub trait GbLexer<'a> {
    fn lexer(&'a self) -> lexer::Lexer<'a>;
}

impl<'a, T> GbLexer<'a> for T
where
    T: AsRef<str> + 'a,
{
    fn lexer(&'a self) -> lexer::Lexer<'a> {
        let input = self.as_ref();
        lexer::Lexer::new(input)
    }
}

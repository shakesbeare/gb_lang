mod position_chars;
mod lexer;

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
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token<'a> {
    literal: &'a str,
    kind: TokenKind,
    location: Location,
}


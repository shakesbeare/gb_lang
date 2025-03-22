mod lexer;
mod position_chars;
#[cfg(test)]
mod tests;

// 1) take in any kind of string to process
// 2) define patterns with regex-like syntax
// 3) output a sequence of tokens

#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
#[error("{msg} at {location}")]
pub struct SyntaxError {
    location: Location,
    msg: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    offset: usize,
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
    /// An invalid token, constructed as part of an error
    Invalid,

    // Main Components
    NumericLiteral,
    StringLiteral,
    Identifier,
    Comment,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
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

    Bang,
    Carat,
    And,
    Deref,
    Equal,
    Plus,
    Minus,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    AngleLeft,
    AngleRight,
    Pipe,

}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token<'a> {
    pub literal: &'a str,
    pub kind: TokenKind,
    pub location: Location,
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

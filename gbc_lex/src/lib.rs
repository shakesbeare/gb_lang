mod lexer;
mod position_chars;
#[cfg(test)]
mod tests;

use gbc_lex_derive::TokenKindExt as TokenKindExtD;

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

pub trait TokenKindExt {
    /// Returns true if the token is a symbol
    fn is_symbol(&self) -> bool;
    /// Returns true if the token is a symbol and is only one character
    fn is_single_length(&self) -> bool;
    /// Returns true if the token is a sybol and is two characters
    fn is_double_length(&self) -> bool;
}

#[derive(TokenKindExtD, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

    // Double length symbols
    #[symbol(double)]
    AndAnd,
    #[symbol(double)]
    EqualEqual,
    #[symbol(double)]
    PlusEqual,
    #[symbol(double)]
    MinusEqual,
    #[symbol(double)]
    StarEqual,
    #[symbol(double)]
    StarStar,
    #[symbol(double)]
    ForwardSlashEqual,
    #[symbol(double)]
    GreaterGreater,
    #[symbol(double)]
    LessLess,
    #[symbol(double)]
    PipePipe,

    // Single length symbols
    #[symbol(single)]
    Bang,
    #[symbol(single)]
    Carat,
    #[symbol(single)]
    And,
    #[symbol(single)]
    Equal,
    #[symbol(single)]
    Plus,
    #[symbol(single)]
    Minus,
    #[symbol(single)]
    Star,
    #[symbol(single)]
    ForwardSlash,
    #[symbol(single)]
    BackSlash,
    #[symbol(single)]
    GreaterThan,
    #[symbol(single)]
    LessThan,
    #[symbol(single)]
    Pipe,
}

#[derive(TokenKindExtD, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

extern crate self as gbc_lex;

pub mod lexer;
mod position_chars;
#[cfg(test)]
mod tests;

use gbc_macros::TokenTypeExt as TokenTypeExtD;
use gbc_shared::Location;

// 1) take in any kind of string to process
// 2) define patterns with regex-like syntax
// 3) output a sequence of tokens

#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
#[error("{msg} at {location}")]
#[readonly::make]
pub struct SyntaxError {
    pub location: Location,
    pub msg: &'static str,
}

pub trait TokenTypeExt {
    /// Returns true if the token is a symbol
    fn is_symbol(&self) -> bool;
    /// Returns true if the token is a symbol and is only one character
    fn is_single_length(&self) -> bool;
    /// Returns true if the token is a sybol and is two characters
    fn is_double_length(&self) -> bool;
}

#[derive(TokenTypeExtD, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenType {
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

#[derive(TokenTypeExtD, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[readonly::make]
pub struct Token<'input> {
    pub literal: &'input str,
    pub ty: TokenType,
    pub location: Location,
}

pub trait GbLexer<'input> {
    fn gb_lexer(&'input self) -> lexer::Lexer<'input>;
}

impl<'input, T> GbLexer<'input> for T
where
    T: AsRef<str> + 'input,
{
    fn gb_lexer(&'input self) -> lexer::Lexer<'input> {
        let input = self.as_ref();
        lexer::Lexer::new(input)
    }
}

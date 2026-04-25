extern crate self as gbc_lex;

pub mod lexer;
mod position_chars;
#[cfg(test)]
mod tests;

use std::sync::Arc;

use gbc_macros::TokenKindExt as TokenKindExtD;
use gbc_shared::Span;

pub trait TokenTypeExt {
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
    Invalid(Arc<str>),

    // Main Components
    NumericLiteral,
    StringLiteral,
    Ident,
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
    #[symbol(single)]
    Semicolon,
    #[symbol(single)]
    Colon,
}

#[derive(TokenKindExtD, Debug, Clone, PartialEq, Eq)]
#[readonly::make]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
    // pub location: Location,
}

pub trait GbLexer<'input> {
    fn gb_lexer(&'input self) -> lexer::Lexer<'input>;
}

impl<'input, T> GbLexer<'input> for T
where
    T: AsRef<str> + 'input,
{
    fn gb_lexer(&'input self) -> lexer::Lexer<'input> {
        lexer::Lexer::new(self.as_ref())
    }
}


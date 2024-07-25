use thiserror::Error;
use crate::token::Token;

#[derive(Debug, Clone, Error)]
pub enum ParserError {
    #[error("{0}")]
    SyntaxError(String),
    #[error("An internal error occured\n{0}")]
    InternalError(String)
}

const UP_ARROWHEAD: &str = "⌃";
const DIVIDER: &str = "--------------------------------";

pub trait ErrorHandler<'a> {
    fn syntax_error(&self, token: Token) -> String;
}

pub struct DefaultErrorHandler<'a> {
    pub input: &'a str,
}

impl<'a> ErrorHandler<'a> for DefaultErrorHandler<'a> {
    fn syntax_error(&self, token: Token) -> String {
        let line = self.input.split('\n').nth(token.location.line - 1).unwrap();
        let padding = " ".repeat(token.location.col - 1);
        format!(
            "\
{DIVIDER}
Syntax Error on line {} at col {}
{DIVIDER}
 {line}
{padding}{UP_ARROWHEAD}
Unexpected {:?}",
            token.location.line, token.location.col, token.literal,
        )
    }
}

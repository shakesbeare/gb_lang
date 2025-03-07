use crate::{interpreter::gb_type::GbError, token::Token};

const UP_ARROWHEAD: &str = "⌃";
const DIVIDER: &str = "――――――――――――――――――――――――――――――――";

pub trait ErrorHandler {
    fn syntax_error(&self, token: Token) -> String;
    fn runtime_error(&self, kind: GbError, token: Token) -> String;
    fn new_input(&mut self, input: String);
}

pub struct DefaultErrorHandler {
    pub input: String,
}

impl ErrorHandler for DefaultErrorHandler {
    fn new_input(&mut self, input: String) {
        self.input = input;
    }

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
Unexpected {:?}
{DIVIDER}",
            token.location.line, token.location.col, token.literal,
        )
    }

    fn runtime_error(&self, kind: GbError, token: Token) -> String {
        let line = self.input.split('\n').nth(token.location.line - 1).unwrap();
        let padding = " ".repeat(token.location.col - 1);
        format!(
            "\
{DIVIDER}
Error on line {} at col {}
{DIVIDER}
 {line}
{padding}{UP_ARROWHEAD}
{}
{DIVIDER}",
            token.location.line, token.location.col, kind
        )
    }
}

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

pub struct REPLErrorHandler {
    input: String,
    default: DefaultErrorHandler,
}

impl REPLErrorHandler {
    pub fn new() -> Self {
        Self {
            input: String::new(),
            default: DefaultErrorHandler {
                input: String::new(),
            },
        }
    }
}

impl Default for REPLErrorHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl ErrorHandler for REPLErrorHandler {
    fn syntax_error(&self, token: Token) -> String {
        self.default.syntax_error(token)
    }

    fn runtime_error(&self, kind: GbError, token: Token) -> String {
        self.default.runtime_error(kind, token)
    }

    fn new_input(&mut self, input: String) {
        self.input.push_str(&input);
        self.default.new_input(self.input.clone());
    }
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
        let line = match self.input.split('\n').nth(token.location.line - 1) {
            Some(l) => l,
            None => {
                tracing::error!("An error occurred: {}", kind);
                tracing::error!("Failed to format the error: Line number could not be found in the input\n\t\tInput: {}\n\t\tLine: {}", self.input, token.location.line - 1);
                std::process::exit(1);
            }
        };
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

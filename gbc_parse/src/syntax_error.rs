use std::sync::Arc;

use gbc_lex::Token;
use gbc_shared::Span;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SyntaxError {
    span: Span,
    kind: SyntaxErrorKind,
}

impl SyntaxError {
    pub fn new(span: Span, kind: SyntaxErrorKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SyntaxErrorKind {
    UnexpectedToken(Arc<Token>),
    UnexpectedEndOfFile,
}

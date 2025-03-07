use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum ParserError {
    #[error("{0}")]
    SyntaxError(String),
    #[error("An internal error occured\n{0}")]
    InternalError(String),
}

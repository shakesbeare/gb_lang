#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum ErrorKind {
    INVALID_NUMERIC_LITERAL,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Error {
    message: String,
    line_number: usize,
    column_number: usize,
    kind: ErrorKind,
}

impl Error {
    pub fn new(message: &str, line_number: usize, column_number: usize, kind: ErrorKind) -> Self {
        Self {
            message: message.to_string(),
            line_number: line_number,
            column_number: column_number,
            kind: kind,
        }
    }
}

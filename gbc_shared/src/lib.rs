use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    pub offset: usize,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, col {}", self.line, self.col)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[readonly::make]
pub struct Span {
    inner: Range<usize>,
}

impl Span {
    /// Creates a new span with start inclusive and end exclusive
    pub fn new(start: usize, end: usize) -> Self {
        Self { inner: start..end }
    }
}

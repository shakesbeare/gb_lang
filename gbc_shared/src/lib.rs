use std::ops::Range;

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

impl std::ops::IndexMut<Span> for str {
    fn index_mut(&mut self, index: Span) -> &mut Self::Output {
        self.index_mut(index.inner)
    }
}

impl std::ops::Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        self.index(index.inner)
    }
}

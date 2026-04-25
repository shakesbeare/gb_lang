extern crate self as gbc_shared;

pub mod traits;
pub mod macros {
    #[allow(unused)]
    pub use gbc_macros::*;
}

use std::ops::Range;

use crate::traits::TreeNode;

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

    pub fn start(&self) -> usize {
        self.inner.start
    }

    pub fn end(&self) -> usize {
        self.inner.end
    }

    pub fn render(&self, input: &str) -> String {
        String::from(&input[self])
    }

    pub fn extend(&mut self, amount: usize) {
        self.inner.end += amount
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

impl std::ops::Index<&Span> for str {
    type Output = str;

    fn index(&self, index: &Span) -> &Self::Output {
        self.index(index.inner.clone())
    }
}

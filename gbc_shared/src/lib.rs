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

impl TreeNode for Span {
    fn repr(&self) -> String {
        format!("Span: {}-{}", self.inner.start, self.inner.end)
    }
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

use std::{iter::Peekable, str::Chars};

#[derive(Clone)]
pub struct PositionChars<'a> {
    iter: Peekable<Chars<'a>>,
    /// The position in characters along the input str
    position: usize,
    original: &'a str,
    /// The human-readable line number of the current position
    line: usize,
    /// The human-readable column number of the current position
    col: usize,
    /// The last char read and consumed by the iterator
    pub(crate) last_char: char,
}

impl<'a> Iterator for PositionChars<'a> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let char_read = self.iter.next()?;
        self.position += 1;
        self.col += 1;
        if char_read == '\n' {
            self.line += 1;
            self.col = 0;
        }
        self.last_char = char_read;
        Some(char_read)
    }
}

impl<'a> From<&'a str> for PositionChars<'a> {
    #[inline]
    fn from(value: &'a str) -> Self {
        Self {
            iter: value.chars().peekable(),
            position: 0,
            original: value,
            line: 0,
            col: 0,
            last_char: '\0',
        }
    }
}

impl<'a> PositionChars<'a> {
    #[inline]
    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        self.iter.peek()
    }

    #[inline]
    pub fn get_slice(&self, start: usize, end: usize) -> &'a str {
        &self.original[start..end]
    }

    #[inline]
    pub fn get_position(&self) -> usize {
        self.position
    }

    #[inline]
    pub fn get_line(&self) -> usize {
        self.line
    }

    #[inline]
    pub fn get_col(&self) -> usize {
        self.col
    }
}

use std::sync::Arc;

use gbc_shared::Span;

use crate::TokenTypeExt;
use crate::{position_chars::PositionChars, Token, TokenKind};

#[derive(Clone)]
#[readonly::make]
pub struct Lexer<'input> {
    pub input: &'input str,
    iter: PositionChars<'input>,
    peeked: Option<Option<Arc<Token>>>,
    peeked_pos: usize,
    pos: usize,
}

impl<'input> Lexer<'input> {
    #[allow(dead_code)]
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            iter: PositionChars::from(input),
            peeked: None,
            pos: 0,
            peeked_pos: 0,
        }
    }

    fn read_until(&mut self, needle: char, second: Option<char>) -> Option<()> {
        loop {
            let char_read = self.iter.next()?;
            if let Some(second) = second {
                let peek = self.iter.peek()?;
                if second != *peek {
                    continue;
                } else {
                    self.iter.next();
                }
            }
            if needle == char_read {
                break;
            }
        }

        Some(())
    }

    fn lex_symbol(&mut self, kind: TokenKind) -> Token {
        if kind.is_double_length() {
            let start = self.pos().unwrap();
            self.iter.next();
            let end = self.peek_pos();
            let span = Span::new(start, end);
            Token { kind, span }
        } else {
            let span = Span::new(self.pos().unwrap(), self.peek_pos());
            Token { kind, span }
        }
    }

    fn lex_numeral(&mut self) -> Token {
        let start = self.pos().unwrap();
        if self.iter.last_char == '0' {
            let Some(next) = self.iter.next() else {
                return Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span::new(start, self.peek_pos()),
                };
            };

            match next {
                'b' => self.consume_binary_digits(),
                'x' => self.consume_hexadecimal_digits(),
                _ => self.consume_decimal_digits(),
            }
        } else {
            self.consume_decimal_digits();
        }
        let end = self.peek_pos();

        Token {
            kind: TokenKind::NumericLiteral,
            span: Span::new(start, end),
        }
    }

    fn consume_decimal_digits(&mut self) {
        while matches!(self.iter.next(), Some('0'..='9' | '.' | '_')) {}
    }

    fn consume_binary_digits(&mut self) {
        while matches!(self.iter.next(), Some('0'..='1' | '.' | '_')) {}
    }

    fn consume_hexadecimal_digits(&mut self) {
        while matches!(
            self.iter.next(),
            Some('0'..='9' | 'a'..='f' | 'A'..='F' | '.' | '_')
        ) {}
    }

    fn lex_identifier(&mut self) -> Token {
        let start = self.pos().unwrap();
        while matches!(
            self.iter.next(),
            Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9')
        ) {}
        let end = self.peek_pos();
        Token {
            kind: try_keyword(self.iter.get_slice(start, end)),
            span: Span::new(start, end),
        }
    }

    fn lex_string_literal(&mut self) -> Token {
        let start = self.pos().unwrap();
        if self.read_until('"', None).is_none() {
            return Token {
                kind: TokenKind::Invalid(Arc::from("\"")),
                span: Span::new(start, self.peek_pos()),
            };
        }
        let end = self.peek_pos();
        Token {
            kind: TokenKind::StringLiteral,
            span: Span::new(start, end),
        }
    }

    fn lex_comment(&mut self) -> Token {
        let start = self.pos().unwrap();
        self.read_until('\n', None);
        let end = self.peek_pos();
        Token {
            kind: TokenKind::Comment,
            span: Span::new(start, end),
        }
    }

    fn lex_block_comment(&mut self) -> Token {
        let start = self.pos().unwrap();
        self.read_until('*', Some('/'));
        let end = self.peek_pos();
        Token {
            kind: TokenKind::Comment,
            span: Span::new(start, end),
        }
    }

    fn lex(&mut self) -> Option<Token> {
        let char_read = self.iter.next()?;
        let peek = self.iter.peek();

        // All infallible tokens must be wrapped in Ok()
        // fallible lexing functions must return Result<Token<'a>, SyntaxError>
        //     and need not be wrapped in Ok()
        Some(match (char_read, peek) {
            (mut c, _) if c.is_whitespace() => {
                while c.is_whitespace() {
                    c = self.iter.next()?;
                }
                self.lex()?
            }
            (c, _) if c.is_numeric() => self.lex_numeral(),
            (c, _) if c.is_alphabetic() || c == '_' => self.lex_identifier(),
            ('"', _) => self.lex_string_literal(),
            ('/', Some('/')) => self.lex_comment(),
            ('/', Some('*')) => self.lex_block_comment(),

            ('(', _) => self.lex_symbol(TokenKind::LParen),
            (')', _) => self.lex_symbol(TokenKind::RParen),
            ('{', _) => self.lex_symbol(TokenKind::LBrace),
            ('}', _) => self.lex_symbol(TokenKind::RBrace),
            ('[', _) => self.lex_symbol(TokenKind::LBracket),
            (']', _) => self.lex_symbol(TokenKind::RBracket),

            ('&', Some('&')) => self.lex_symbol(TokenKind::AndAnd),
            ('=', Some('=')) => self.lex_symbol(TokenKind::EqualEqual),
            ('+', Some('=')) => self.lex_symbol(TokenKind::PlusEqual),
            ('-', Some('=')) => self.lex_symbol(TokenKind::MinusEqual),
            ('*', Some('=')) => self.lex_symbol(TokenKind::StarEqual),
            ('*', Some('*')) => self.lex_symbol(TokenKind::StarStar),
            ('/', Some('=')) => self.lex_symbol(TokenKind::ForwardSlashEqual),
            ('>', Some('>')) => self.lex_symbol(TokenKind::GreaterGreater),
            ('<', Some('<')) => self.lex_symbol(TokenKind::LessLess),
            ('|', Some('|')) => self.lex_symbol(TokenKind::PipePipe),

            ('!', _) => self.lex_symbol(TokenKind::Bang),
            ('^', _) => self.lex_symbol(TokenKind::Carat),
            ('&', _) => self.lex_symbol(TokenKind::And),
            ('=', _) => self.lex_symbol(TokenKind::Equal),
            ('+', _) => self.lex_symbol(TokenKind::Plus),
            ('-', _) => self.lex_symbol(TokenKind::Minus),
            ('*', _) => self.lex_symbol(TokenKind::Star),
            ('/', _) => self.lex_symbol(TokenKind::ForwardSlash),
            ('\\', _) => self.lex_symbol(TokenKind::BackSlash),
            ('>', _) => self.lex_symbol(TokenKind::GreaterThan),
            ('<', _) => self.lex_symbol(TokenKind::LessThan),
            ('|', _) => self.lex_symbol(TokenKind::Pipe),

            _ => {
                // skipping forward to the next space gives us a pretty reasonable chance
                // to recover lexing the rest of the file
                // A more robust strategy is probably warranted
                let start = self.pos().unwrap();
                self.read_until(' ', None);
                let end = self.peek_pos();
                Token {
                    kind: TokenKind::Invalid(Arc::from("valid char")),
                    span: Span::new(start, end),
                }
            }
        })
    }

    #[inline]
    pub fn pos(&self) -> Option<usize> {
        self.iter.pos()
    }

    #[inline]
    pub fn peek_pos(&self) -> usize {
        self.iter.peek_pos()
    }

    #[inline]
    pub fn peek(&mut self) -> Option<&Arc<Token>> {
        match self.peeked {
            Some(ref v) => v.as_ref(),
            None => {
                self.peeked = Some(self.next());
                self.peeked.as_ref().unwrap().as_ref()
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Arc<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(v) => {
                self.pos = self.iter.pos().unwrap();
                v
            }
            None => self.lex().map(Arc::from),
        }
    }
}

impl<'a> std::iter::FusedIterator for Lexer<'a> {}

/// Returns the appropriate keyword token if the &str matches a keyword
/// Otherwise, returns `TokenKind::Identifier`
#[inline]
fn try_keyword(literal: &str) -> TokenKind {
    match literal {
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "return" => TokenKind::Return,
        "fn" => TokenKind::Fn,
        "while" => TokenKind::While,
        "for" => TokenKind::For,
        "continue" => TokenKind::Continue,
        "break" => TokenKind::Break,
        "struct" => TokenKind::Struct,
        "enum" => TokenKind::Enum,
        _ => TokenKind::Identifier,
    }
}

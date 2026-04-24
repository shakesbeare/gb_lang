use gbc_shared::Span;

use crate::TokenTypeExt;
use crate::{position_chars::PositionChars, SyntaxError, Token, TokenKind};

#[derive(Clone)]
#[readonly::make]
pub struct Lexer<'input> {
    pub input: &'input str,
    iter: PositionChars<'input>,
    errors: Vec<SyntaxError>,
}

impl<'input> Lexer<'input> {
    #[allow(dead_code)]
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            iter: PositionChars::from(input),
            errors: vec![],
        }
    }

    fn syntax_error(&mut self, span: Span, message: &'static str) -> SyntaxError {
        let e = SyntaxError { span, msg: message };

        #[cfg(feature = "eager_error_printing")]
        {
            println!("{}", e);
        }
        self.errors.push(e.clone());
        e
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
            let start = self.iter.pos().unwrap();
            self.iter.next();
            let end = self.iter.peek_pos();
            let span = Span::new(start, end);
            Token { kind, span }
        } else {
            let span = Span::new(self.iter.pos().unwrap(), self.iter.peek_pos());
            Token { kind, span }
        }
    }

    fn lex_numeral(&mut self) -> Token {
        let start = self.iter.pos().unwrap();
        if self.iter.last_char == '0' {
            let Some(next) = self.iter.next() else {
                return Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span::new(start, self.iter.peek_pos()),
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
        let end = self.iter.peek_pos();

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
        let start = self.iter.pos().unwrap();
        while matches!(
            self.iter.next(),
            Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9')
        ) {}
        let end = self.iter.peek_pos();
        Token {
            kind: try_keyword(self.iter.get_slice(start, end)),
            span: Span::new(start, end),
        }
    }

    fn lex_string_literal(&mut self) -> Result<Token, SyntaxError> {
        let start = self.iter.pos().unwrap();
        if self.read_until('"', None).is_none() {
            return Err(self.syntax_error(
                Span::new(start, self.iter.pos().unwrap()),
                "Unterminated string literal",
            ));
        }
        let end = self.iter.peek_pos();
        Ok(Token {
            kind: TokenKind::StringLiteral,
            span: Span::new(start, end),
        })
    }

    fn lex_comment(&mut self) -> Token {
        let start = self.iter.pos().unwrap();
        self.read_until('\n', None);
        let end = self.iter.peek_pos();
        Token {
            kind: TokenKind::Comment,
            span: Span::new(start, end),
        }
    }

    fn lex_block_comment(&mut self) -> Token {
        let start = self.iter.pos().unwrap();
        self.read_until('*', Some('/'));
        let end = self.iter.peek_pos();
        Token {
            kind: TokenKind::Comment,
            span: Span::new(start, end),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        let char_read = self.iter.next()?;
        let peek = self.iter.peek();

        // All infallible tokens must be wrapped in Ok()
        // fallible lexing functions must return Result<Token<'a>, SyntaxError>
        //     and need not be wrapped in Ok()
        let lex_result = match (char_read, peek) {
            (mut c, _) if c.is_whitespace() => {
                while c.is_whitespace() {
                    c = self.iter.next()?;
                }
                self.next()?
            }
            (c, _) if c.is_numeric() => Ok(self.lex_numeral()),
            (c, _) if c.is_alphabetic() || c == '_' => Ok(self.lex_identifier()),
            ('"', _) => self.lex_string_literal(),
            ('/', Some('/')) => Ok(self.lex_comment()),
            ('/', Some('*')) => Ok(self.lex_block_comment()),

            ('(', _) => Ok(self.lex_symbol(TokenKind::LParen)),
            (')', _) => Ok(self.lex_symbol(TokenKind::RParen)),
            ('{', _) => Ok(self.lex_symbol(TokenKind::LBrace)),
            ('}', _) => Ok(self.lex_symbol(TokenKind::RBrace)),
            ('[', _) => Ok(self.lex_symbol(TokenKind::LBracket)),
            (']', _) => Ok(self.lex_symbol(TokenKind::RBracket)),

            ('&', Some('&')) => Ok(self.lex_symbol(TokenKind::AndAnd)),
            ('=', Some('=')) => Ok(self.lex_symbol(TokenKind::EqualEqual)),
            ('+', Some('=')) => Ok(self.lex_symbol(TokenKind::PlusEqual)),
            ('-', Some('=')) => Ok(self.lex_symbol(TokenKind::MinusEqual)),
            ('*', Some('=')) => Ok(self.lex_symbol(TokenKind::StarEqual)),
            ('*', Some('*')) => Ok(self.lex_symbol(TokenKind::StarStar)),
            ('/', Some('=')) => Ok(self.lex_symbol(TokenKind::ForwardSlashEqual)),
            ('>', Some('>')) => Ok(self.lex_symbol(TokenKind::GreaterGreater)),
            ('<', Some('<')) => Ok(self.lex_symbol(TokenKind::LessLess)),
            ('|', Some('|')) => Ok(self.lex_symbol(TokenKind::PipePipe)),

            ('!', _) => Ok(self.lex_symbol(TokenKind::Bang)),
            ('^', _) => Ok(self.lex_symbol(TokenKind::Carat)),
            ('&', _) => Ok(self.lex_symbol(TokenKind::And)),
            ('=', _) => Ok(self.lex_symbol(TokenKind::Equal)),
            ('+', _) => Ok(self.lex_symbol(TokenKind::Plus)),
            ('-', _) => Ok(self.lex_symbol(TokenKind::Minus)),
            ('*', _) => Ok(self.lex_symbol(TokenKind::Star)),
            ('/', _) => Ok(self.lex_symbol(TokenKind::ForwardSlash)),
            ('\\', _) => Ok(self.lex_symbol(TokenKind::BackSlash)),
            ('>', _) => Ok(self.lex_symbol(TokenKind::GreaterThan)),
            ('<', _) => Ok(self.lex_symbol(TokenKind::LessThan)),
            ('|', _) => Ok(self.lex_symbol(TokenKind::Pipe)),

            _ => {
                // skipping forward to the next space gives us a pretty reasonable chance
                // to recover lexing the rest of the file
                // A more robust strategy is probably warranted
                let start = self.iter.pos().unwrap();
                self.read_until(' ', None);
                let end = self.iter.peek_pos();
                Err(self.syntax_error(Span::new(start, end), "Unexpected character"))
            }
        };

        Some(lex_result)
    }
}

impl<'a> std::iter::FusedIterator for Lexer<'a> {}

/// Returns the appropriate keyword token if the &str matches a keyword
/// Otherwise, returns `TokenKind::Identifier`
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

use crate::{position_chars::PositionChars, Location, SyntaxError, Token, TokenKind};

#[derive(Clone)]
pub struct Lexer<'a> {
    iter: PositionChars<'a>,
    errors: Vec<SyntaxError>,
    print_errors: bool,
}

impl<'a> Lexer<'a> {
    #[allow(dead_code)]
    pub fn new(input: &'a str) -> Self {
        Self {
            iter: PositionChars::from(input),
            errors: vec![],
            print_errors: true,
        }
    }

    #[allow(dead_code)]
    pub fn print_errors(&mut self, state: bool) {
        self.print_errors = state;
    }

    fn syntax_error(&mut self, message: &'static str) -> SyntaxError {
        let e = SyntaxError {
            location: Location {
                offset: self.iter.get_position(),
                line: self.iter.get_line(),
                col: self.iter.get_col(),
            },
            msg: message,
        };
        if self.print_errors {
            println!("{}", e);
        }
        self.errors.push(e.clone());
        e
    }

    fn read_until(&mut self, needle: char, second: Option<char>) -> Option<()> {
        loop {
            let char_read = self.iter.next()?;
            if second.is_some() {
                let peek = self.iter.peek()?;
                if second.unwrap() != *peek {
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

    fn lex_single(&mut self, kind: TokenKind) -> Token<'a> {
        Token {
            literal: self
                .iter
                .get_slice(self.iter.get_position() - 1, self.iter.get_position()),
            kind,
            location: Location {
                offset: self.iter.get_position() - 1,
                line: self.iter.get_line(),
                col: self.iter.get_col() - 1,
            },
        }
    }

    fn lex_numeral(&mut self) -> Token<'a> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        if self.iter.last_char == '0' {
            let Some(next) = self.iter.next() else {
                return Token {
                    literal: self.iter.get_slice(start, self.iter.get_position()),
                    kind: TokenKind::NumericLiteral,
                    location: Location { offset: start, line, col },
                };
            };

            match next {
                'b' => self.consume_binary_digits(),
                'x' => self.consume_hexadecimal_digits(),
                _=> self.consume_decimal_digits(),
            }
        } else {
            self.consume_decimal_digits();
        }
        let end = self.iter.get_position();

        Token {
            literal: self.iter.get_slice(start, end),
            kind: TokenKind::NumericLiteral,
            location: Location { offset: start, line, col },
        }
    }

    fn consume_decimal_digits(&mut self) {
        while matches!(self.iter.next(), Some('0'..='9' | '.' | '_')) {}
    }

    fn consume_binary_digits(&mut self){
        while matches!(self.iter.next(), Some('0'..='1' | '.' | '_')) {}
    }

    fn consume_hexadecimal_digits(&mut self) {
        while matches!(
            self.iter.next(),
            Some('0'..='9' | 'a'..='f' | 'A'..='F' | '.' | '_')
        ) {}
    }

    fn lex_identifier(&mut self) -> Token<'a> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        while matches!(
            self.iter.next(),
            Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9')
        ) {}
        let end = self.iter.get_position();
        println!("{}", self.iter.get_slice(start, end));
        Token {
            literal: self.iter.get_slice(start, end),
            kind: try_keyword(self.iter.get_slice(start, end)),
            location: Location { offset: start, line, col },
        }
    }

    fn lex_string_literal(&mut self) -> Result<Token<'a>, SyntaxError> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        if self.read_until('"', None).is_none() {
            return Err(self.syntax_error("Unterminated string literal"));
        }
        let end = self.iter.get_position();
        Ok(Token {
            literal: self.iter.get_slice(start, end),
            kind: TokenKind::StringLiteral,
            location: Location { offset: start, line, col },
        })
    }

    fn lex_comment(&mut self) -> Token<'a> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        self.read_until('\n', None);
        let end = self.iter.get_position();
        Token {
            literal: self.iter.get_slice(start, end),
            kind: TokenKind::Comment,
            location: Location { offset: start, line, col },
        }
    }

    fn lex_block_comment(&mut self) -> Token<'a> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        self.read_until('*', Some('/'));
        let end = self.iter.get_position();
        Token {
            literal: self.iter.get_slice(start, end),
            kind: TokenKind::Comment,
            location: Location { offset: start, line, col },
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, SyntaxError>;

    /// Currently, returning None means `both` to stop iterating and that a syntax error occurred
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
            },
            (c, _) if c.is_numeric() => Ok(self.lex_numeral()),
            (c, _) if c.is_alphabetic() || c == '_' => Ok(self.lex_identifier()),
            ('"', _) => self.lex_string_literal(),
            ('/', Some('/')) => Ok(self.lex_comment()),
            ('/', Some('*')) => Ok(self.lex_block_comment()),
            ('(', None) => Ok(self.lex_single(TokenKind::LParen)),
            (')', None) => Ok(self.lex_single(TokenKind::RParen)),
            ('{', None) => Ok(self.lex_single(TokenKind::LBrace)),
            ('}', None) => Ok(self.lex_single(TokenKind::RBrace)),
            ('[', None) => Ok(self.lex_single(TokenKind::LBracket)),
            (']', None) => Ok(self.lex_single(TokenKind::RBracket)),
            ('!', None) => Ok(self.lex_single(TokenKind::Bang)),
            ('&', None) => Ok(self.lex_single(TokenKind::And)),
            ('^', None) => Ok(self.lex_single(TokenKind::Carat)),
            ('=', None) => Ok(self.lex_single(TokenKind::Equal)),
            ('+', None) => Ok(self.lex_single(TokenKind::Plus)),
            ('-', None) => Ok(self.lex_single(TokenKind::Minus)),
            ('*', None) => Ok(self.lex_single(TokenKind::Multiply)),
            ('/', None) => Ok(self.lex_single(TokenKind::Divide)),
            ('>', None) => Ok(self.lex_single(TokenKind::GreaterThan)),
            ('<', None) => Ok(self.lex_single(TokenKind::LessThan)),
            ('|', None) => Ok(self.lex_single(TokenKind::Pipe)),
            _ => {
                println!("\n{}", char_read);
                todo!();
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

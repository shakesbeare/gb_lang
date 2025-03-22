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
                line: self.iter.get_line(),
                col: self.iter.get_col() - 1,
            },
        }
    }

    fn lex_double(&mut self, kind: TokenKind) -> Token<'a> {
        self.iter.next();
        Token {
            literal: self
                .iter
                .get_slice(self.iter.get_position() - 2, self.iter.get_position()),
            kind,
            location: Location {
                line: self.iter.get_line(),
                col: self.iter.get_col() - 2,
            },
        }
    }

    fn lex_decimal(&mut self) -> Token<'a> {
        // must account for the initially read character
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        while matches!(self.iter.next(), Some('0'..='9' | '.' | '_')) {}
        let end = self.iter.get_position();
        Token {
            literal: self.iter.get_slice(start, end),
            kind: TokenKind::DecimalLiteral,
            location: Location { line, col },
        }
    }

    fn lex_hexadecimal(&mut self) -> Result<Token<'a>, SyntaxError> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        self.iter.next();
        while matches!(
            self.iter.next(),
            Some('0'..='9' | 'a'..='f' | 'A'..='F' | '.' | '_')
        ) {}
        let end = self.iter.get_position();
        if end - start <= 2 {
            return Err(self.syntax_error("Malformed Hexadecimal Literal"));
        }
        Ok(Token {
            literal: self.iter.get_slice(start, end),
            kind: TokenKind::HexadecimalLiteral,
            location: Location { line, col },
        })
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
        Token {
            literal: self.iter.get_slice(start, end),
            kind: try_keyword(self.iter.get_slice(start, end)),
            location: Location { line, col },
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
            location: Location { line, col },
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
            location: Location { line, col },
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
            location: Location { line, col },
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
            (c, _) if c.is_whitespace() => self.next()?,
            (c, _) if c == '0' && peek.is_some() && *peek.unwrap() == 'x' => {
                self.lex_hexadecimal()
            }
            (c, _) if c.is_numeric() => Ok(self.lex_decimal()),
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
            ('!', None) => Ok(self.lex_single(TokenKind::Not)),
            ('&', None) => Ok(self.lex_single(TokenKind::Ref)),
            ('^', None) => Ok(self.lex_single(TokenKind::Deref)),
            ('=', None) => Ok(self.lex_single(TokenKind::Assign)),
            ('+', None) => Ok(self.lex_single(TokenKind::Add)),
            ('-', None) => Ok(self.lex_single(TokenKind::Subtract)),
            ('*', None) => Ok(self.lex_single(TokenKind::Multiply)),
            ('/', None) => Ok(self.lex_single(TokenKind::Divide)),
            ('>', None) => Ok(self.lex_single(TokenKind::GreaterThan)),
            ('<', None) => Ok(self.lex_single(TokenKind::LessThan)),
            ('>', Some('=')) => Ok(self.lex_double(TokenKind::GreaterEquals)),
            ('<', Some('=')) => Ok(self.lex_double(TokenKind::LessEquals)),
            ('=', Some('=')) => Ok(self.lex_double(TokenKind::Equal)),
            ('!', Some('=')) => Ok(self.lex_double(TokenKind::NotEqual)),
            _ => todo!(),
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

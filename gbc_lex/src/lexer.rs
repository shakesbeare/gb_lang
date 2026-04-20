use crate::{position_chars::PositionChars, SyntaxError, Token, TokenType};
use gbc_shared::Location;
use crate::TokenTypeExt;

#[derive(Clone)]
#[readonly::make]
pub struct Lexer<'input> {
    pub input: &'input str,
    iter: PositionChars<'input>,
    errors: Vec<SyntaxError>,
    print_errors: bool,
}

impl<'input> Lexer<'input> {
    #[allow(dead_code)]
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
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

    fn lex_symbol(&mut self, kind: TokenType) -> Token<'input> {
        let extra_offset = if kind.is_double_length() { 1 } else { 0 };
        let token = Token {
            literal: self
                .iter
                .get_slice(self.iter.get_position() - 1, self.iter.get_position() + extra_offset),
            ty: kind,
            location: Location {
                offset: self.iter.get_position() - 1,
                line: self.iter.get_line(),
                col: self.iter.get_col() - 1,
            },
        };

        // this is kinda janky but saves the copy
        if extra_offset == 1 {
            self.iter.next(); // pass over the second half of the token
        }

        token
    }

    fn lex_numeral(&mut self) -> Token<'input> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        if self.iter.last_char == '0' {
            let Some(next) = self.iter.next() else {
                return Token {
                    literal: self.iter.get_slice(start, self.iter.get_position()),
                    ty: TokenType::NumericLiteral,
                    location: Location {
                        offset: start,
                        line,
                        col,
                    },
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
        let end = self.iter.get_position();

        Token {
            literal: self.iter.get_slice(start, end),
            ty: TokenType::NumericLiteral,
            location: Location {
                offset: start,
                line,
                col,
            },
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

    fn lex_identifier(&mut self) -> Token<'input> {
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
            ty: try_keyword(self.iter.get_slice(start, end)),
            location: Location {
                offset: start,
                line,
                col,
            },
        }
    }

    fn lex_string_literal(&mut self) -> Result<Token<'input>, SyntaxError> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        if self.read_until('"', None).is_none() {
            return Err(self.syntax_error("Unterminated string literal"));
        }
        let end = self.iter.get_position();
        Ok(Token {
            literal: self.iter.get_slice(start, end),
            ty: TokenType::StringLiteral,
            location: Location {
                offset: start,
                line,
                col,
            },
        })
    }

    fn lex_comment(&mut self) -> Token<'input> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        self.read_until('\n', None);
        let end = self.iter.get_position();
        Token {
            literal: self.iter.get_slice(start, end),
            ty: TokenType::Comment,
            location: Location {
                offset: start,
                line,
                col,
            },
        }
    }

    fn lex_block_comment(&mut self) -> Token<'input> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        self.read_until('*', Some('/'));
        let end = self.iter.get_position();
        Token {
            literal: self.iter.get_slice(start, end),
            ty: TokenType::Comment,
            location: Location {
                offset: start,
                line,
                col,
            },
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, SyntaxError>;

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

            ('(', _) => Ok(self.lex_symbol(TokenType::LParen)),
            (')', _) => Ok(self.lex_symbol(TokenType::RParen)),
            ('{', _) => Ok(self.lex_symbol(TokenType::LBrace)),
            ('}', _) => Ok(self.lex_symbol(TokenType::RBrace)),
            ('[', _) => Ok(self.lex_symbol(TokenType::LBracket)),
            (']', _) => Ok(self.lex_symbol(TokenType::RBracket)),

            ('&', Some('&')) => Ok(self.lex_symbol(TokenType::AndAnd)),
            ('=', Some('=')) => Ok(self.lex_symbol(TokenType::EqualEqual)),
            ('+', Some('=')) => Ok(self.lex_symbol(TokenType::PlusEqual)),
            ('-', Some('=')) => Ok(self.lex_symbol(TokenType::MinusEqual)),
            ('*', Some('=')) => Ok(self.lex_symbol(TokenType::StarEqual)),
            ('*', Some('*')) => Ok(self.lex_symbol(TokenType::StarStar)),
            ('/', Some('=')) => Ok(self.lex_symbol(TokenType::ForwardSlashEqual)),
            ('>', Some('>')) => Ok(self.lex_symbol(TokenType::GreaterGreater)),
            ('<', Some('<')) => Ok(self.lex_symbol(TokenType::LessLess)),
            ('|', Some('|')) => Ok(self.lex_symbol(TokenType::PipePipe)),

            ('!',  _) => Ok(self.lex_symbol(TokenType::Bang)),
            ('^',  _) => Ok(self.lex_symbol(TokenType::Carat)),
            ('&',  _) => Ok(self.lex_symbol(TokenType::And)),
            ('=',  _) => Ok(self.lex_symbol(TokenType::Equal)),
            ('+',  _) => Ok(self.lex_symbol(TokenType::Plus)),
            ('-',  _) => Ok(self.lex_symbol(TokenType::Minus)),
            ('*',  _) => Ok(self.lex_symbol(TokenType::Star)),
            ('/',  _) => Ok(self.lex_symbol(TokenType::ForwardSlash)),
            ('\\', _) => Ok(self.lex_symbol(TokenType::BackSlash)),
            ('>',  _) => Ok(self.lex_symbol(TokenType::GreaterThan)),
            ('<',  _) => Ok(self.lex_symbol(TokenType::LessThan)),
            ('|',  _) => Ok(self.lex_symbol(TokenType::Pipe)),

            _ => {
                // skipping forward to the next space gives us a pretty reasonable chance
                // to recover lexing the rest of the file
                self.read_until(' ', None);
                Err(self.syntax_error("Unexpected character"))
            }
        };

        Some(lex_result)
    }
}

impl<'a> std::iter::FusedIterator for Lexer<'a> {}

/// Returns the appropriate keyword token if the &str matches a keyword
/// Otherwise, returns `TokenKind::Identifier`
fn try_keyword(literal: &str) -> TokenType {
    match literal {
        "true" => TokenType::True,
        "false" => TokenType::False,
        "return" => TokenType::Return,
        "fn" => TokenType::Fn,
        "while" => TokenType::While,
        "for" => TokenType::For,
        "continue" => TokenType::Continue,
        "break" => TokenType::Break,
        "struct" => TokenType::Struct,
        "enum" => TokenType::Enum,
        _ => TokenType::Identifier,
    }
}

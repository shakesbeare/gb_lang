mod position_chars;

// 1) take in any kind of string to process
// 2) define patterns with regex-like syntax
// 3) output a sequence of tokens

use position_chars::PositionChars;

#[derive(Debug, Clone, thiserror::Error)]
#[error("{msg} at {location}")]
pub struct SyntaxError {
    location: Location,
    msg: &'static str,
}

#[derive(Clone)]
pub struct Lexer<'a> {
    iter: PositionChars<'a>,
    errors: Vec<SyntaxError>,
    print_errors: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            iter: PositionChars::from(input),
            errors: vec![],
            print_errors: true
        }
    }

    pub fn print_errors(&mut self, state: bool) {
        self.print_errors = state;
    }

    fn syntax_error(&mut self, message: &'static str) {
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
        self.errors.push(e);
    }

    fn lex_decimal(&mut self) -> Token<'a> {
        // must account for the initially read character
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        loop {
            let char_read = self.iter.next();
            match char_read {
                Some(c) if c.is_numeric() || c == '.' || c == '_' => (),
                _ => break,
            }
        }
        let end = self.iter.get_position();
        Token {
            literal: self.iter.get_slice(start, end),
            kind: TokenKind::DecimalLiteral,
            location: Location {
                line: self.iter.get_line(),
                col,
            },
        }
    }

    fn lex_hexadecimal(&mut self) -> Option<Token<'a>> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        self.iter.next();
        loop {
            let char_read = self.iter.next();
            match char_read {
                Some(c) if c.is_ascii_hexdigit() || c == '.' || c == '_' => (),
                _ => {
                    if self.iter.get_position() - start <= 2 {
                        self.syntax_error("Malformed Hexadecimal Literal");
                        return None;
                    }
                    break;
                }
            }
        }
        let end = self.iter.get_position();
        Some(Token {
            literal: self.iter.get_slice(start, end),
            kind: TokenKind::HexadecimalLiteral,
            location: Location {
                line: self.iter.get_line(),
                col,
            },
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let char_read = self.iter.next()?;
        let peek = self.iter.peek();

        let tok = match char_read {
            c if c == '0' && peek.is_some() && *peek.unwrap() == 'x' => {
                self.lex_hexadecimal()?
            }
            c if c.is_numeric() => self.lex_decimal(),
            _ => todo!(),
        };

        Some(tok)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    line: usize,
    col: usize,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, col {}", self.line, self.col)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    DecimalLiteral,
    HexadecimalLiteral,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token<'a> {
    literal: &'a str,
    kind: TokenKind,
    location: Location,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_int_literal() {
        let input = "12345";
        let tok = Lexer::new(input).next().unwrap();
        assert_eq!(tok.literal, input);
        assert_eq!(tok.kind, TokenKind::DecimalLiteral);
        assert_eq!(tok.location, Location { line: 0, col: 0 })
    }

    #[test]
    fn lex_float_literal() {
        let input = "123.45";
        let tok = Lexer::new(input).next().unwrap();
        assert_eq!(tok.literal, input);
        assert_eq!(tok.kind, TokenKind::DecimalLiteral);
        assert_eq!(tok.location, Location { line: 0, col: 0 });
    }

    #[test]
    fn lex_numeral_separators() {
        let input = "123_45";
        let tok = Lexer::new(input).next().unwrap();
        assert_eq!(tok.literal, input);
        assert_eq!(tok.kind, TokenKind::DecimalLiteral);
        assert_eq!(tok.location, Location { line: 0, col: 0 })
    }

    #[test]
    fn lex_hexadecimal_literal() {
        let input = "0xff";
        let tok = Lexer::new(input).next().unwrap();
        assert_eq!(tok.literal, input);
        assert_eq!(tok.kind, TokenKind::HexadecimalLiteral);
        assert_eq!(tok.location, Location { line: 0, col: 0 })
    }

    #[test]
    #[should_panic]
    fn lex_invalid_hex() {
        let input = "0x";
        let tok = Lexer::new(input).next().unwrap();
        assert_eq!(tok.literal, input);
        assert_eq!(tok.kind, TokenKind::HexadecimalLiteral);
        assert_eq!(tok.location, Location { line: 0, col: 0 })
    }
}

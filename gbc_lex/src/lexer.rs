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
        while matches!(self.iter.next(), Some('0'..='9' | '.' | '_')) {}
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
        while matches!(
            self.iter.next(),
            Some('0'..='9' | 'a'..='f' | 'A'..='F' | '.' | '_')
        ) {}
        let end = self.iter.get_position();
        if end - start <= 2 {
            self.syntax_error("Malformed Hexadecimal Literal");
            return None;
        }
        Some(Token {
            literal: self.iter.get_slice(start, end),
            kind: TokenKind::HexadecimalLiteral,
            location: Location {
                line: self.iter.get_line(),
                col,
            },
        })
    }

    fn lex_identifier(&mut self) -> Token<'a> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        while matches!(
            self.iter.next(),
            Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9')
        ) {}
        let end = self.iter.get_position();
        Token {
            literal: self.iter.get_slice(start, end),
            kind: TokenKind::Identifier,
            location: Location {
                line: self.iter.get_line(),
                col,
            },
        }
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
            c if c.is_alphabetic() => self.lex_identifier(),
            _ => todo!(),
        };

        Some(tok)
    }
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

    #[test]
    fn lex_identifier() {
        let input = "word123";
        let tok = Lexer::new(input).next().unwrap();
        assert_eq!(tok.literal, input);
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(tok.location, Location { line: 0, col: 0 })
    }
}

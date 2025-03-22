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

    fn lex_hexadecimal(&mut self) -> Option<Token<'a>> {
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
            self.syntax_error("Malformed Hexadecimal Literal");
            return None;
        }
        Some(Token {
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
            kind: TokenKind::Identifier,
            location: Location { line, col },
        }
    }

    fn lex_string_literal(&mut self) -> Option<Token<'a>> {
        let start = self.iter.get_position() - 1;
        let col = self.iter.get_col() - 1;
        let line = self.iter.get_line();
        self.read_until('"', None);
        let end = self.iter.get_position();
        Some(Token {
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
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let char_read = self.iter.next()?;
        let peek = self.iter.peek();

        let tok = match (char_read, peek) {
            (c, _) if c.is_whitespace() => self.next()?,
            (c, _) if c == '0' && peek.is_some() && *peek.unwrap() == 'x' => {
                self.lex_hexadecimal()?
            }
            (c, _) if c.is_numeric() => self.lex_decimal(),
            (c, _) if c.is_alphabetic() || c == '_' => self.lex_identifier(),
            ('"', _) => self.lex_string_literal()?,
            ('/', Some('/')) => self.lex_comment(),
            ('/', Some('*')) => self.lex_block_comment(),
            _ => todo!(),
        };

        Some(tok)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn simple_lex_test(input: &str, expected: TokenKind) {
        let tok = Lexer::new(input).next().unwrap();
        assert_eq!(tok.literal, input);
        assert_eq!(tok.kind, expected);
        assert_eq!(tok.location, Location { line: 0, col: 0 })
    }

    #[test]
    fn lex_int_literal() {
        simple_lex_test("12345", TokenKind::DecimalLiteral);
    }

    #[test]
    fn lex_float_literal() {
        simple_lex_test("12.345", TokenKind::DecimalLiteral);
    }

    #[test]
    fn lex_numeral_separators() {
        simple_lex_test("12_345", TokenKind::DecimalLiteral);
    }

    #[test]
    fn lex_hexadecimal_literal() {
        simple_lex_test("0xff", TokenKind::HexadecimalLiteral);
    }

    #[test]
    #[should_panic]
    fn lex_invalid_hex() {
        simple_lex_test("0x", TokenKind::HexadecimalLiteral);
    }

    #[test]
    fn lex_identifier() {
        simple_lex_test("_word123", TokenKind::Identifier);
    }

    #[test]
    fn lex_string_literal() {
        simple_lex_test(
            "\"Hello, World! #1234567890+[{(&=)}]*\"",
            TokenKind::StringLiteral,
        );
    }

    #[test]
    fn lex_comment() {
        simple_lex_test("// this is a comment", TokenKind::Comment);
    }

    #[test]
    fn lex_block_comment() {
        simple_lex_test(
            "/* this is a block
            comment */",
            TokenKind::Comment,
        );
    }

    #[test]
    fn skip_whitespace() {
        let input = "    word     \n\t\r1234";
        let mut l = Lexer::new(input);
        let first = l.next().unwrap();
        let second = l.next().unwrap();
        assert_eq!(first.kind, TokenKind::Identifier);
        assert_eq!(second.kind, TokenKind::DecimalLiteral);
    }
}

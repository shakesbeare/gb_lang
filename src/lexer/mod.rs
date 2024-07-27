mod tests;

use crate::token::Point;
use crate::token::Token;
use crate::token::TokenKind;

use anyhow::Result;
use std::fs::File;

use std::io::{BufRead, BufReader, Read};

const KEYWORDS: [&str; 9] = [
    "true", "false", "if", "for", "while", "let", // "use", "restrict",
    "else", "fn", "return",
];

#[derive(Debug, Eq, PartialEq)]
pub enum ReadCharStatus {
    Reading(char),
    Eof,
}

#[derive(Debug, Eq, PartialEq)]
pub enum LexStatus {
    Reading {
        token: Token,
    },
    SyntaxError {
        failed_lexeme: String,
        location: Point,
        unexpected_char: char,
    },
    Eof,
}

pub struct Lexer<T: Read> {
    pub next_token: Option<Token>,

    pub token_stream: Vec<Token>,

    reader: BufReader<T>,
    buf: [u8; 1],

    pub line: usize,
    pub col: usize,
}

impl<'a> From<&'a [u8]> for Lexer<&'a [u8]> {
    fn from(value: &'a [u8]) -> Self {
        Lexer {
            next_token: None,
            token_stream: vec![],

            reader: BufReader::with_capacity(1, value),
            buf: [0],

            line: 1,
            col: 1,
        }
    }
}

impl From<File> for Lexer<File> {
    fn from(file: File) -> Lexer<File> {
        Lexer {
            next_token: None,
            token_stream: vec![],

            reader: BufReader::with_capacity(1, file),
            buf: [0],

            line: 1,
            col: 1,
        }
    }
}

impl Lexer<File> {
    pub fn open_file<S: Into<String>>(filename: S) -> Result<Lexer<File>> {
        let file = File::open(filename.into())?;
        Ok(Lexer::from(file))
    }
}

impl<T: Read> Lexer<T> {
    pub fn new_input(&mut self, value: T) {
        self.next_token = None;
        self.token_stream.clear();
        self.reader = BufReader::with_capacity(1, value);
        self.buf = [0];
        self.line = 0;
        self.col = 0;
    }

    /// Lexes the entire input buffer, consuming it.
    #[allow(dead_code)]
    pub fn lex_all(&mut self) {
        loop {
            match self.lex() {
                LexStatus::Reading { .. } => (),
                LexStatus::SyntaxError { .. } => (),
                LexStatus::Eof => break,
            }
        }
    }

    /// Consumes and returns the next character of the input buffer
    /// Will return Status::Eof if the end of the file has been reached.
    /// or ReadCharStatus::Reading(char) if a character was read successfully.
    fn get_char(&mut self) -> ReadCharStatus {
        // get the next token
        let Ok(bytes_read) = self.reader.read(&mut self.buf[..]) else {
            panic!("unable to read input buffer");
        };

        // Ensure that EOF has not been reached
        if bytes_read == 0 {
            return ReadCharStatus::Eof;
        }

        // convert the bytes to char
        let char_read = self.buf[0] as char;

        // advance column number
        self.col += 1;

        return ReadCharStatus::Reading(char_read);
    }

    /// Checks the next character of the input buffer without consuming it
    fn peek(&mut self) -> char {
        let buf = self.reader.fill_buf().expect("unable to read input buffer");
        if buf.is_empty() {
            return '\0';
        }
        return buf[0] as char;
    }

    /// Consumes a portion of the input buffer
    pub fn lex(&mut self) -> LexStatus {
        // flush next_token, next_lexeme, and next_point to the streams
        if let Some(next_token) = &self.next_token {
            self.token_stream.push(next_token.clone());
            self.next_token = None;
        }

        // create this variable in this outer scope so we can use it later
        // \0 is the null character
        // because the type `char` cannot be empty
        #[allow(unused_assignments)]
        let mut char_read = '\0';

        // some parts of the lexing process need to look at the next char
        // but not consume it, so this character has to be passed back into the
        // lexer
        let ReadCharStatus::Reading(c) = self.get_char() else {
            self.lex_eof();
            return LexStatus::Eof;
        };
        char_read = c;

        // consume the buffer and build next_token and next_lexeme
        match char_read {
            _ if char_read.is_alphabetic() || char_read == '_' => {
                self.lex_word(char_read)
            } // end identifier
            '0'..='9' => self.lex_number(char_read), // end number literal
            '"' | '\'' => self.lex_string_literal(char_read), // end string literal
            '(' => {
                let token =
                    Token::new(char_read, TokenKind::LParen, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end L paren
            ')' => {
                let token =
                    Token::new(char_read, TokenKind::RParen, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end R paren
            '{' => {
                let token =
                    Token::new(char_read, TokenKind::LBrace, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end L brace
            '}' => {
                let token =
                    Token::new(char_read, TokenKind::RBrace, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end R brace
            '[' => {
                let token =
                    Token::new(char_read, TokenKind::LBracket, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end L bracket
            ']' => {
                let token =
                    Token::new(char_read, TokenKind::RBracket, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end R bracket
            '=' => {
                if let Some(status) = self.lex_peek(char_read, '=', TokenKind::Equals) {
                    return status;
                }
                let token =
                    Token::new(char_read, TokenKind::Assign, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            '+' => {
                let token =
                    Token::new(char_read, TokenKind::Add, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end +
            '-' => {
                let token =
                    Token::new(char_read, TokenKind::Subtract, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end -
            '*' => {
                if let Some(status) =
                    self.lex_peek(char_read, '*', TokenKind::Exponentiate)
                {
                    return status;
                }
                let token =
                    Token::new(char_read, TokenKind::Multiply, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end *
            '/' => {
                if self.peek() == '*' {
                    self.get_char(); // pass over the *
                    self.ignore_until('*', Some('/'));
                    return self.lex();
                } else if self.peek() == '/' {
                    self.ignore_until('\n', None);
                    return self.lex();
                } else {
                    let token =
                        Token::new(char_read, TokenKind::Divide, (self.line, self.col));
                    self.next_token = Some(token);
                    return LexStatus::Reading {
                        token: self.next_token.clone().unwrap(),
                    };
                }
            } // end /
            '>' => {
                let token = Token::new(
                    char_read,
                    TokenKind::GreaterThan,
                    (self.line, self.col),
                );
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            '<' => {
                let token =
                    Token::new(char_read, TokenKind::LessThan, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            '!' => {
                if let Some(status) =
                    self.lex_peek(char_read, '=', TokenKind::NotEquals)
                {
                    return status;
                }

                let token =
                    Token::new(char_read, TokenKind::Bang, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            ';' => {
                let token =
                    Token::new(char_read, TokenKind::Semicolon, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            ',' => {
                let token =
                    Token::new(char_read, TokenKind::Comma, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            '\n' => {
                self.col = 1;
                self.line += 1;
                let token =
                    Token::new(char_read, TokenKind::Eol, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            ' ' => {
                return self.lex();
            }
            '#' => {
                if self.peek() == '!' && self.line == 1 {
                    self.ignore_until('\n', None);
                    return self.lex();
                } else {
                    return LexStatus::SyntaxError {
                        failed_lexeme: c.to_string(),
                        location: Point::from((self.line, self.col)),
                        unexpected_char: c,
                    };
                }
            }
            c => {
                return LexStatus::SyntaxError {
                    failed_lexeme: c.to_string(),
                    location: Point::from((self.line, self.col)),
                    unexpected_char: c,
                };
            }
        }
    }

    fn lex_string_literal(
        &mut self,
        char_read /* delimiter, " or ' */: char,
    ) -> LexStatus {
        let mut lexeme = String::new();
        while self.peek() != '\0' {
            let next_char = self.peek();
            match next_char {
                _ if next_char == char_read => {
                    // found the matching " or '
                    self.get_char();
                    break;
                }
                _ if next_char == '\\' => {
                    // escaped characters
                    // TODO: handle escape sequences
                    if let ReadCharStatus::Reading(c) = self.get_char() {
                        lexeme.push(c);
                    }
                    self.get_char();
                }
                c => {
                    // anything else
                    self.get_char();
                    lexeme.push(c);
                }
            }
        }
        self.next_token = Some(Token::new(
            lexeme,
            TokenKind::StringLiteral,
            (self.line, self.col),
        ));

        LexStatus::Reading {
            token: self.next_token.clone().unwrap(),
        }
    }

    fn lex_number(&mut self, char_read: char) -> LexStatus {
        let mut lexeme = String::from(char_read);
        while self.peek() != '\0' {
            let next_char = self.peek();
            match next_char {
                c if next_char.is_numeric() || next_char == '.' => {
                    self.get_char();
                    lexeme.push(c);
                }
                _ if next_char.is_whitespace()
                    || ["{", "}", "(", ")", "[", "]"]
                        .contains(&char_read.to_string().as_str()) =>
                {
                    break;
                }
                _ if next_char.is_alphabetic() => {
                    return LexStatus::SyntaxError {
                        failed_lexeme: lexeme.clone(),
                        location: Point::from((self.line, self.col)),
                        unexpected_char: next_char,
                    };
                }
                _ => {
                    break;
                }
            }
        }
        self.next_token = match lexeme {
            _ if lexeme.contains('.') => Some(Token::new(
                lexeme,
                TokenKind::FloatLiteral,
                (self.line, self.col),
            )),
            _ => Some(Token::new(
                lexeme,
                TokenKind::IntLiteral,
                (self.line, self.col),
            )),
        };

        return LexStatus::Reading {
            token: self.next_token.clone().unwrap(),
        };
    }

    fn lex_word(&mut self, char_read: char) -> LexStatus {
        let mut lexeme = String::from(char_read);
        while self.peek() != '\0' {
            let next_char = self.peek();
            match next_char {
                c if next_char.is_alphanumeric() || next_char == '_' => {
                    self.get_char(); // consume the peeked character
                    lexeme.push(c);
                }
                _ if ['\'', '\"'].contains(&next_char) => {
                    return LexStatus::SyntaxError {
                        failed_lexeme: lexeme.clone(),
                        location: Point::from((self.line, self.col)),
                        unexpected_char: next_char,
                    };
                }
                _ => {
                    break;
                }
            }
        }
        if KEYWORDS.contains(&lexeme.as_str()) {
            let token_kind: TokenKind = (&lexeme).into();
            self.next_token =
                Some(Token::new(lexeme, token_kind, (self.line, self.col)));
        } else {
            self.next_token = Some(Token::new(
                lexeme,
                TokenKind::Identifier,
                (self.line, self.col),
            ));
        }
        return LexStatus::Reading {
            token: self.next_token.clone().unwrap(),
        };
    }

    /// Advances the reader and returns the token_kind if the next char matches
    /// Otherwise, returns none
    fn lex_peek(
        &mut self,
        cur_char: char,
        required_char: char,
        token_kind: TokenKind,
    ) -> Option<LexStatus> {
        if self.peek() == required_char {
            self.get_char();
            let token = Token::new(
                format!("{}{}", cur_char, required_char),
                token_kind,
                (self.line, self.col),
            );
            self.next_token = Some(token);
            return Some(LexStatus::Reading {
                token: self.next_token.clone().unwrap(),
            });
        } else {
            return None;
        };
    }

    /// Ignores input until the given character, or until the given character followed by the
    /// second
    /// Returns `None` if lexer did not reach Eof during this operation
    ///
    fn ignore_until(
        &mut self,
        stop_char: char,
        second_stop_char: Option<char>,
    ) -> Option<LexStatus> {
        loop {
            let ReadCharStatus::Reading(char) = self.get_char() else {
                self.lex_eof();
                return Some(LexStatus::Eof);
            };

            if let Some(second) = second_stop_char {
                if char == stop_char && self.peek() == second {
                    self.get_char(); // pass over the second char
                    break;
                }
            } else if char == stop_char {
                break;
            }
        }

        None
    }

    fn lex_eof(&mut self) {
        let token = Token::new("\0", TokenKind::Eof, (self.line, self.col));
        self.next_token = Some(token);
    }
}

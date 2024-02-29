use crate::token::Token;
use crate::token::TokenKind;
use crate::token::Point;

use anyhow::Result;
use std::fs::File;

use std::io::{BufRead, BufReader, Read};

const KEYWORDS: [&str; 11] = [
    "true", "false", "if", "for", "while", "let", "use", "restrict", "use",
    "else", "fn",
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

#[allow(dead_code)]

pub struct Lexer<T: Read> {
    pub next_token: Option<Token>,
    current_word: String,

    pub token_stream: Vec<Token>,

    reader: BufReader<T>,
    buf: [u8; 1],

    pub line: usize,
    pub col: usize,
}

impl From<&'static [u8]> for Lexer<&[u8]> {
    fn from(value: &'static [u8]) -> Self {
        Lexer {
            next_token: None,
            token_stream: vec![],
            current_word: String::new(),

            reader: BufReader::with_capacity(1, value),
            buf: [0],

            line: 0,
            col: 0,
        }
    }
}

impl From<File> for Lexer<File> {
    fn from(file: File) -> Lexer<File> {
        Lexer {
            next_token: None,
            token_stream: vec![],

            current_word: String::new(),

            reader: BufReader::with_capacity(1, file),
            buf: [0],

            line: 0,
            col: 0,
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

        // If current_word contains only a space,
        // replace it with nothing
        if &self.current_word == " " {
            self.current_word = String::new();
        }

        // consume the buffer and build next_token and next_lexeme
        match char_read {
            _ if char_read.is_alphabetic() || char_read == '_' => {
                let mut lexeme = String::from(char_read);
                while self.peek() != '\0' {
                    let next_char = self.peek();
                    match next_char {
                        c if next_char.is_alphanumeric()
                            || next_char == '_' =>
                        {
                            self.get_char(); // consume the peeked character
                            lexeme.push(c);
                        }
                        _ if ['\'', '\"'].contains(&next_char) => {
                            return LexStatus::SyntaxError {
                                failed_lexeme: lexeme.clone(),
                                location: Point::from((self.line, self.col)),
                                unexpected_char: next_char,
                            };
                        } // syntax error
                        _ => {
                            break;
                        }
                    }
                }

                if KEYWORDS.contains(&lexeme.as_str()) {
                    match &lexeme {
                        _ if ["true", "false"].contains(&lexeme.as_str()) => {
                            let token = Token::new(lexeme, TokenKind::Boolean, (self.line, self.col));

                            self.next_token = Some(token)
                        }
                        _ => self.next_token = Some(Token::new(lexeme, TokenKind::Keyword, (self.line, self.col))),
                    }
                } else {
                    self.next_token = Some(Token::new(lexeme, TokenKind::Identifier, (self.line, self.col)));
                }
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end identifier
            '0'..='9' => {
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
                    _ if lexeme.contains('.') => Some(Token::new(lexeme, TokenKind::FloatLiteral, (self.line, self.col))),
                    _ => Some(Token::new(lexeme, TokenKind::IntLiteral, (self.line, self.col))),
                };
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end number literal
            '"' | '\'' => {
                let mut lexeme = String::from(char_read);
                while self.peek() != '\0' {
                    let next_char = self.peek();
                    match next_char {
                        c if next_char == char_read => {
                            self.get_char();
                            lexeme.push(c);
                            break;
                        }
                        _ if next_char == '\\' => {
                            self.get_char(); // pass over the \
                            self.get_char(); // pass over the next character
                        }
                        c => {
                            self.get_char();
                            lexeme.push(c);
                        }
                    }
                }

                if !lexeme.ends_with(char_read)
                    && lexeme.chars().nth_back(1).unwrap() != '\\'
                {
                    return LexStatus::SyntaxError {
                        failed_lexeme: lexeme.clone(),
                        location: Point::from((self.line, self.col)),
                        unexpected_char: lexeme.chars().last().unwrap(),
                    };
                } else {
                    self.next_token = Some(Token::new(lexeme, TokenKind::StringLiteral, (self.line, self.col)));
                    return LexStatus::Reading {
                        token: self.next_token.clone().unwrap()
                    };
                }
            } // end string literal
            '(' => {
                let token = Token::new(char_read, TokenKind::LParen, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end L paren
            ')' => {
                let token = Token::new(char_read, TokenKind::RParen, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end R paren
            '{' => {
                let token = Token::new(char_read, TokenKind::LBrace, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end L brace
            '}' => {
                let token = Token::new(char_read, TokenKind::RBrace, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end R brace
            '[' => {
                let token = Token::new(char_read, TokenKind::LBracket, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end L bracket
            ']' => {
                let token = Token::new(char_read, TokenKind::RBracket, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end R bracket
            '=' => {
                let token = Token::new(char_read, TokenKind::OpAssign, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            '+' => {
                let token = Token::new(char_read, TokenKind::OpAdd, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end +
            '-' => {
                let token = Token::new(char_read, TokenKind::OpSub, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            } // end -
            '*' => {
                if self.peek() == '*' {
                    self.get_char();
                    let token = Token::new("**", TokenKind::OpExp, (self.line, self.col));
                    self.next_token = Some(token);
                    return LexStatus::Reading {
                        token: self.next_token.clone().unwrap(),
                    };
                } else {
                    let token = Token::new(char_read, TokenKind::OpMul, (self.line, self.col));
                    self.next_token = Some(token);
                    return LexStatus::Reading {
                        token: self.next_token.clone().unwrap(),
                    };
                }
            } // end *
            '/' => {
                if self.peek() == '*' {
                    self.get_char(); // pass over the *
                    loop {
                        let ReadCharStatus::Reading(char) = self.get_char()
                        else {
                            self.lex_eof();
                            return LexStatus::Eof;
                        };

                        if char == '*' && self.peek() == '/' {
                            self.get_char();
                            break;
                        }
                    }
                    return self.lex();
                } else if self.peek() == '/' {
                    loop {
                        let ReadCharStatus::Reading(char) = self.get_char()
                        else {
                            self.lex_eof();
                            return LexStatus::Eof;
                        };

                        if char == '\n' {
                            break;
                        }
                    }
                    return self.lex();
                } else {
                    let token = Token::new(char_read, TokenKind::OpDiv, (self.line, self.col));
                    self.next_token = Some(token);
                    return LexStatus::Reading {
                        token: self.next_token.clone().unwrap(),
                    };
                }
            } // end /
            '>' => {
                let token = Token::new(char_read, TokenKind::OpGt, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            '<' => {
                let token = Token::new(char_read, TokenKind::OpLt, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            '!' => {
                let token = Token::new(char_read, TokenKind::OpBang, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            ';' => {
                let token = Token::new(char_read, TokenKind::Semicolon, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            ',' => {
                let token = Token::new(char_read, TokenKind::Comma, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            '\n' => {
                self.col = 0;
                self.line += 1;
                let token = Token::new(char_read, TokenKind::Eol, (self.line, self.col));
                self.next_token = Some(token);
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                };
            }
            ' ' => {
                return self.lex();
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

    fn lex_eof(&mut self) {
        let token = Token::new("\0", TokenKind::Eof, (self.line, self.col));
        self.next_token = Some(token);
    }
}

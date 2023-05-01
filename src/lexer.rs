use crate::token::Token;

use anyhow::Result;
use std::fs::File;

use std::io::{BufRead, BufReader, Read};

const KEYWORDS: [&str; 8] = [
    "true", "false", "if", "for", "while", "let", "use", "restrict",
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
        lexeme: String,
    },
    SyntaxError {
        failed_lexeme: String,
        location: Point,
        unexpected_char: char,
    },
    Eof,
}

#[allow(dead_code)]
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Point {
    line: usize,
    col: usize,
}

impl From<(usize, usize)> for Point {
    fn from(value: (usize, usize)) -> Self {
        Point {
            line: value.0,
            col: value.1,
        }
    }
}

pub struct Lexer<T: Read> {
    pub next_token: Option<Token>,
    pub next_lexeme: Option<String>,
    pub next_point: Option<Point>,
    current_word: String,

    pub token_stream: Vec<Token>,
    pub lexeme_stream: Vec<String>,
    pub point_stream: Vec<Point>,

    reader: BufReader<T>,
    buf: [u8; 1],
    need_new_char: bool,

    pub line: usize,
    pub col: usize,
}

impl From<&'static [u8]> for Lexer<&[u8]> {
    fn from(value: &'static [u8]) -> Self {
        Lexer {
            next_token: None,
            next_lexeme: None,
            next_point: None,

            token_stream: vec![],
            lexeme_stream: vec![],
            point_stream: vec![],

            current_word: String::new(),

            reader: BufReader::with_capacity(1, value),
            buf: [0],
            need_new_char: true,

            line: 0,
            col: 0,
        }
    }
}

impl From<File> for Lexer<File> {
    fn from(file: File) -> Lexer<File> {
        Lexer {
            next_token: None,
            next_lexeme: None,
            next_point: None,

            token_stream: vec![],
            lexeme_stream: vec![],
            point_stream: vec![],

            current_word: String::new(),

            reader: BufReader::with_capacity(1, file),
            buf: [0],
            need_new_char: true,

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
    /// Lexes the entire input buffer, consuming it.
    #[allow(dead_code)]
    pub fn lex_all(&mut self)  {
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
    /// or Status::Reading(char) if a character was read successfully.
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

        if let Some(next_lexeme) = &self.next_lexeme {
            self.lexeme_stream.push(next_lexeme.clone());
            self.next_lexeme = None;
        }

        if let Some(next_point) = &self.next_point {
            self.point_stream.push(next_point.clone());
            self.next_point = None;
        }

        // if something went horribly wrong, crash the program
        // the streams should always be the same length
        if self.token_stream.len() != self.lexeme_stream.len() {
            panic!("Token Stream and Lexeme Stream have different lengths");
        }

        // create this variable in this outer scope so we can use it later
        // \0 is the null character
        // because the type `char` cannot be empty
        #[allow(unused_assignments)]
        let mut char_read = '\0';

        // some parts of the lexing process need to look at the next char
        // but not consume it, so this character has to be passed back into the
        // lexer
        if self.need_new_char {
            let ReadCharStatus::Reading(c) = self.get_char() else {
                self.lex_eof();
                return LexStatus::Eof;
            };
            char_read = c;
        } else {
            char_read = self
                .current_word
                .chars()
                .next()
                .expect("need new char is false, but current_word is empty");
        }

        // reset the flag to default
        self.need_new_char = true;

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
                        c if next_char.is_alphanumeric() => {
                            self.get_char(); // consume the peeked character
                            lexeme.push(c);
                        }
                        _ if vec!['\'', '\"'].contains(&next_char) => {
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

                self.next_lexeme = Some(lexeme.clone());
                self.next_token = Some(Token::Identifier);
                return LexStatus::Reading {
                    token: Token::Identifier,
                    lexeme: lexeme.clone(),
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
                            || vec!["{", "}", "(", ")", "[", "]"]
                                .contains(&char_read.to_string().as_str()) =>
                        {
                            break;
                        }
                        _ => {
                            return LexStatus::SyntaxError {
                                failed_lexeme: lexeme.clone(),
                                location: Point::from((self.line, self.col)),
                                unexpected_char: next_char,
                            };
                        } // syntax error
                    }
                }
                self.next_lexeme = Some(lexeme.clone());
                self.next_token = match lexeme {
                    _ if lexeme.contains('.') => Some(Token::FloatLiteral),
                    _ => Some(Token::IntLiteral),
                };
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
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
                    self.next_token = Some(Token::StringLiteral);
                    self.next_lexeme = Some(lexeme.clone());
                    return LexStatus::Reading {
                        token: Token::StringLiteral,
                        lexeme,
                    };
                }
            } // end string literal
            '(' => {
                self.next_token = Some(Token::LParen);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            } // end L paren
            ')' => {
                self.next_token = Some(Token::RParen);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            } // end R paren
            '{' => {
                self.next_token = Some(Token::LBrace);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            } // end L brace
            '}' => {
                self.next_token = Some(Token::RBrace);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            } // end R brace
            '[' => {
                self.next_token = Some(Token::LBracket);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            } // end L bracket
            ']' => {
                self.next_token = Some(Token::RBracket);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            } // end R bracket
            '=' => {
                self.next_token = Some(Token::OpAssign);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            }
            '+' => {
                self.next_token = Some(Token::OpAdd);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            } // end +
            '-' => {
                self.next_token = Some(Token::OpSub);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            } // end -
            '*' => {
                if self.peek() == '*' {
                    self.get_char();
                    self.next_token = Some(Token::OpExp);
                    self.next_lexeme = Some(String::from("**"));
                    self.next_point = Some(Point::from((self.line, self.col)));
                    return LexStatus::Reading {
                        token: self.next_token.clone().unwrap(),
                        lexeme: self.next_lexeme.clone().unwrap(),
                    };
                } else {
                    self.next_token = Some(Token::OpMul);
                    self.next_lexeme = Some(String::from(char_read));
                    self.next_point = Some(Point::from((self.line, self.col)));
                    return LexStatus::Reading {
                        token: self.next_token.clone().unwrap(),
                        lexeme: self.next_lexeme.clone().unwrap(),
                    };
                }
            } // end *
            '/' => {
                if self.peek() == '*' {
                    self.get_char(); // pass over the *
                    loop {
                        let ReadCharStatus::Reading(char) = self.get_char() else {
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
                        let ReadCharStatus::Reading(char) = self.get_char() else {
                            self.lex_eof();
                            return LexStatus::Eof;
                        };

                        if char == '\n' {
                            break;
                        }
                    }
                    return self.lex();
                } else {
                    self.next_token = Some(Token::OpDiv);
                    self.next_lexeme = Some(String::from(char_read));
                    return LexStatus::Reading {
                        token: self.next_token.clone().unwrap(),
                        lexeme: self.next_lexeme.clone().unwrap(),
                    };
                }
            } // end /
            '>' => {
                self.next_token = Some(Token::OpGt);
                self.next_lexeme = Some(String::from(char_read));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            }
            '<' => {
                self.next_token = Some(Token::OpLt);
                self.next_lexeme = Some(String::from(char_read));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            }
            ';' => {
                self.next_token = Some(Token::Semicolon);
                self.next_lexeme = Some(String::from(char_read));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
            }
            '\n' => {
                self.col = 0;
                self.line += 1;
                self.next_token = Some(Token::Eol);
                self.next_lexeme = Some(String::from("\n"));
                return LexStatus::Reading {
                    token: self.next_token.clone().unwrap(),
                    lexeme: self.next_lexeme.clone().unwrap(),
                };
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
        self.next_token = Some(Token::Eof);
        self.next_lexeme = Some("\0".to_string());
    }
}

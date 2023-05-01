use crate::token::Token;

use anyhow::Result;
use std::fs::File;

use std::io::{BufReader, Read, BufRead};

const KEYWORDS: [&str; 8] = [
    "true", 
    "false",
    "if", 
    "for",
    "while",
    "let",
    "use",
    "restrict",
];

#[derive(Debug, Eq, PartialEq)]
pub enum ReadCharStatus {
    Reading(char),
    Eof,
}

#[derive(Debug, Eq, PartialEq)]
pub enum LexStatus {
    Reading {token: Token, lexeme: String },
    SyntaxError {failed_lexeme: String, location: Point },
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
    pub fn lex_all(&mut self) -> Result<()> {
        while let LexStatus::Reading { .. } = self.lex() {
        }
        return Ok(());
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
            char_read = self.current_word.chars().next().expect("need new char is false, but current_word is empty");
        }

        // reset the flag to default
        self.need_new_char = true;

        // If current_word contains only a space, 
        // replace it with nothing
        if &self.current_word == " " {
            self.current_word = String::new();
        }

        // lex the character read
        // some characters need to read additional characters to be properly
        // lexed i.e. strings, identifiers and numbers
        match char_read {
            _ if char_read.is_alphabetic() => {
                let mut lexeme = String::from(char_read);
                while self.peek() != '\0' {
                    let next_char = self.peek();
                    match self.peek() {
                        c if next_char.is_alphanumeric() => {
                            self.get_char(); // consume the peeked character
                            lexeme.push(c);
                        }
                        _ if next_char.is_whitespace() 
                            || vec!["{", "}", "(", ")", "[", "]"].contains(&char_read.to_string().as_str()) => {
                                break;
                            }
                        _ => {
                            return LexStatus::SyntaxError {failed_lexeme: lexeme.clone(), location: Point::from((self.line, self.col))};
                        } // syntax error
                    }
                }

                self.next_lexeme = Some(lexeme.clone());
                self.next_token = Some(Token::Identifier);
                return LexStatus::Reading{ token: Token::Identifier, lexeme: lexeme.clone() };
            } // end identifier
            '0'..='9' => {
                let mut lexeme = String::from(char_read);
                while self.peek() != '\0' {
                    let next_char = self.peek();
                    match self.peek() {
                        c if next_char.is_numeric()
                            || next_char == '.' => {
                                self.get_char();
                                lexeme.push(c);
                            }
                        _ if next_char.is_whitespace() 
                            || vec!["{", "}", "(", ")", "[", "]"].contains(&char_read.to_string().as_str()) => {
                                break;
                            }
                        _ => {
                            return LexStatus::SyntaxError {failed_lexeme: lexeme.clone(), location: Point::from((self.line, self.col))};
                        } // syntax error
                    }

                }
                self.next_lexeme = Some(lexeme.clone());
                self.next_token = match lexeme {
                    _ if lexeme.contains('.') => { Some(Token::FloatLiteral) }
                    _  => { Some(Token::IntLiteral) }
                }
            } // end number literal
            '"' | '\'' => {
                self.current_word += &String::from(char_read);
                self.lex_string(char_read);
            } // end string literal
            '(' => {
                self.next_token = Some(Token::LParen);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
            } // end L paren
            ')' => {
                self.next_token = Some(Token::RParen);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
                self.current_word = String::new();
            } // end R paren
            '{' => {
                self.next_token = Some(Token::LBrace);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
            } // end L brace
            '}' => {
                self.next_token = Some(Token::RBrace);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
            } // end R brace
            '[' => {
                self.next_token = Some(Token::LBracket);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
            } // end L bracket
            ']' => {
                self.next_token = Some(Token::RBracket);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
            } // end R bracket
            '=' => {
                self.next_token = Some(Token::OpAssign);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
            }
            '+' => {
                self.next_token = Some(Token::OpAdd);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
            } // end +
            '-' => {
                self.next_token = Some(Token::OpSub);
                self.next_lexeme = Some(String::from(char_read));
                self.next_point = Some(Point::from((self.line, self.col)));
            } // end -
            '*' => {
                 if self.peek() == '*' {
                    self.get_char();
                    self.next_token = Some(Token::OpExp);
                    self.next_lexeme = Some(String::from("**"));
                    self.next_point = Some(Point::from((self.line, self.col)));
                } else {
                    self.next_token = Some(Token::OpMul);
                    self.next_lexeme = Some(String::from(char_read));
                    self.next_point = Some(Point::from((self.line, self.col)));
                }
            } // end *
            '/' => {
                if self.peek() == '*'{
                    self.current_word = String::new();
                    loop {
                        let ReadCharStatus::Reading(char) = self.get_char() else {
                            self.lex_eof();
                            return LexStatus::Eof;
                        };

                        if char == '*' && self.peek() == '/'{
                            self.get_char();
                            break;
                        }
                    }
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
                } else {
                    self.next_token = Some(Token::OpDiv);
                    self.next_lexeme = Some(String::from(char_read));
                }
            } // end / 
            '>' => {
                self.next_token = Some(Token::OpGt);
                self.next_lexeme = Some(String::from(char_read));
            }
            '<' => {
                self.next_token = Some(Token::OpLt);
                self.next_lexeme = Some(String::from(char_read));
            }
            ';' => {
                self.next_token = Some(Token::Semicolon);
                self.next_lexeme = Some(String::from(char_read));
                self.current_word = String::new();
            }
            '\n' => {
                self.col = 0;
                self.line += 1;
                self.current_word = String::new();
                self.next_token = Some(Token::Eol);
                self.next_lexeme = Some(String::from("\n"));
            }
            _ => {
                self.lex();
            }
        }
        return LexStatus::Reading{token: self.next_token.clone().unwrap(), lexeme: self.next_lexeme.clone().unwrap()};
    }

   fn lex_number(&mut self) {
        let ReadCharStatus::Reading(char_read) = self.get_char() else {
            self.next_token = Some(Token::IntLiteral);
            self.next_lexeme = Some(self.current_word.clone());
            self.current_word = String::new();
            return
        };

        match char_read {
            '0'..='9' | '.' => {
                self.current_word += &String::from(char_read);
                self.lex_number();
            }
            _ => {
                if self.current_word.contains('.') { 
                    self.next_token = Some(Token::FloatLiteral)
                } else {
                    self.next_token = Some(Token::IntLiteral);
                }

                self.next_lexeme = Some(self.current_word.clone());
                self.need_new_char = false;
                self.current_word = String::from(char_read);
            }
        }
    }

    fn lex_string(&mut self, c: char) {
        let ReadCharStatus::Reading(char_read) = self.get_char() else {
            self.next_token = Some(Token::StringLiteral);
            self.next_lexeme = Some(self.current_word.clone());
            self.current_word = String::new();
            return
        };
        match char_read {
           char_read if char_read == c => {
                // string ends
                self.current_word += &String::from(char_read);
                self.next_token = Some(Token::StringLiteral);
                self.next_lexeme = Some(self.current_word.clone());
                self.current_word = String::new();
                return
            }
            _ => {
                // string continues
                self.current_word += &String::from(char_read);
                self.lex_string(c);
            }
        }
    }

    fn lex_eof(&mut self) {
        self.next_token = Some(Token::Eof);
        self.next_lexeme = Some("\0".to_string());
    }
}


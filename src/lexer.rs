use crate::token::Token;

use anyhow::{Result, Error};
use std::fs::File;

use std::io::{BufReader, Read};


pub enum Status {
    Reading(char),
    EOF,
}

#[derive(Clone)]
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

pub struct Lexer {
    pub next_token: Option<Token>,
    pub next_lexeme: Option<String>,
    pub next_point: Option<Point>,
    current_word: String,

    pub token_stream: Vec<Token>,
    pub lexeme_stream: Vec<String>,
    pub point_stream: Vec<Point>,

    reader: BufReader<File>,
    buf: [u8; 1],
    need_new_char: bool,

    line: usize,
    col: usize,
}

impl Lexer {
    pub fn new<S: Into<String>>(file: S) -> Result<Lexer> {
        Ok(Lexer {
            next_token: None,
            next_lexeme: None,
            next_point: None,

            token_stream: vec![],
            lexeme_stream: vec![],
            point_stream: vec![],

            current_word: String::new(),

            reader: BufReader::new(File::open(file.into())?),
            buf: [0],
            need_new_char: true,

            line: 0,
            col: 0,
        })
    }

    pub fn lex_all(&mut self) -> Result<()> {
        loop {
            match self.lex() {
                Status::Reading(_) => continue,
                Status::EOF => break,
            }
        }
        return Ok(());
    }

    fn get_char(&mut self) -> Status {
        // get the next token
        let Ok(bytes_read) = self.reader.read(&mut self.buf[..]) else {
            panic!("unable to read input buffer");
        };

        // Ensure that EOF has not been reached
        if bytes_read == 0 {
            return Status::EOF;
        }

        // convert the bytes to char
        let char_read = self.buf[0] as char;

        // advance column number
        self.col += 1;

        return Status::Reading(char_read);
    }

    /// Consumes a portion of the input buffer and sets next_token and next_lexeme
    pub fn lex(&mut self) -> Status {
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
        if !(self.token_stream.len() == self.lexeme_stream.len()) {
            panic!("Token Stream and Lexeme Stream have different lengths");
        }

        // create this variable in this outer scope so we can use it later
        // \0 is the null character
        // because the type `char` cannot be empty
        #[allow(unused_assignments)]
        let mut char_read = '\0';

        // some parts of theh lexing process need to look at the next char
        // but not consume it, so this character has to be passed back into the
        // lexer
        if self.need_new_char {
            let Status::Reading(c) = self.get_char() else {
                return Status::EOF;
            };
            char_read = c;
        } else {
            char_read = self.current_word.chars().nth(0).expect("need new char is false, but current_word is empty");
        }

        // reset the flag to default
        self.need_new_char = true;

        // lex the character read
        // some characters need to read additional characters to be properly
        // lexed i.e. strings, identifiers and numbers
        match char_read {
            'a'..='z' | 'A'..='Z' | '_'=> {
                self.current_word += &String::from(char_read);
                self.lex_identifier();
            } // end identifier
            '0'..='9' => {
                self.current_word += &String::from(char_read);
                self.lex_number();
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
                let Some(last_lexeme) = self.lexeme_stream.last() else {
                    self.next_token = Some(Token::OpMul);
                    self.next_lexeme = Some(String::from(char_read));
                    self.next_point = Some(Point::from((self.line, self.col)));
                    return Status::Reading(char_read);
                };

                if last_lexeme == "/" {
                    self.lexeme_stream.pop();
                    self.token_stream.pop();
                    self.point_stream.pop();

                    self.current_word = String::new();
                    loop {
                        let Status::Reading(char) = self.get_char() else {
                                break;
                        };

                        if char == '*' {
                            let Status::Reading(n_char) = self.get_char() else {
                                break;
                            };

                            if n_char == '/' {
                                break;
                            }
                        }
                    }
                } else {
                    self.next_token = Some(Token::OpDiv);
                    self.next_lexeme = Some(String::from(char_read));
                }
            } // end *
            '/' => {
                let Some(last_lexeme) = self.lexeme_stream.last() else {
                    self.next_token = Some(Token::OpDiv);
                    self.next_lexeme = Some(String::from(char_read));
                    return Status::Reading(char_read);
                };
                
                if last_lexeme == "/" {
                    self.lexeme_stream.pop();
                    self.token_stream.pop();

                    self.current_word = String::new();
                    loop {
                        let Status::Reading(char) = self.get_char() else {
                                break;
                        };

                        if char == '\n' {
                            self.current_word = String::from("\n");
                            self.need_new_char = false;
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
                self.next_token = Some(Token::EOL);
                self.next_lexeme = Some(String::from(char_read));
            }
            '\n' => {
                self.col = 0;
                self.line += 1;
                self.current_word = String::new();
            }
            _ => {
                self.lex();
            }
        }
        return Status::Reading(char_read);
    }

    fn lex_identifier(&mut self) {
        let Status::Reading(char_read) = self.get_char() else {
            self.next_token = Some(Token::Identifier);
            self.next_lexeme = Some(self.current_word.clone());
            self.current_word = String::new();
            return 
        };

        match char_read {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                // identifier continues
                self.current_word += &String::from(char_read);
                self.lex_identifier();
            }
            _ => {
                self.next_token = Some(Token::Identifier);
                self.next_lexeme = Some(self.current_word.clone());
                self.need_new_char = false;
                if char_read != '\n' {
                    self.current_word = String::from(char_read);
                } else {
                    self.current_word = String::from("\n");
                }
            }
        }
    }

   fn lex_number(&mut self) {
        let Status::Reading(char_read) = self.get_char() else {
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
                if self.current_word.contains(".") { 
                    self.next_token = Some(Token::FloatLiteral)
                } else {
                    self.next_token = Some(Token::IntLiteral);
                }

                self.next_lexeme = Some(self.current_word.clone());
                self.need_new_char = false;
                
                if char_read != '\n' {
                    self.current_word = String::from(char_read);
                } else {
                    self.current_word = String::from("\n");
                }
            }
        }
    }

    fn lex_string(&mut self, c: char) {
        let Status::Reading(char_read) = self.get_char() else {
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
}


use crate::token::Token;

use anyhow::{Result, Error};
use std::fs::File;

use std::io::{BufReader, Read};


pub enum Status {
    Reading(char),
    EOF,
}

pub struct Lexer {
    next_token: Option<Token>,
    next_lexeme: Option<String>,
    current_word: String,

    pub token_stream: Vec<Token>,
    pub lexeme_stream: Vec<String>,

    reader: BufReader<File>,
    buf: [u8; 1],
}

impl Lexer {
    pub fn new<S: Into<String>>(file: S) -> Result<Lexer> {
        Ok(Lexer {
            next_token: None,
            next_lexeme: None,

            token_stream: vec![],
            lexeme_stream: vec![],

            current_word: String::new(),

            reader: BufReader::new(File::open(file.into())?),
            buf: [0],
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

        // begin state machine
        let char_read = self.buf[0] as char;

        return Status::Reading(char_read);
    }


    /// Consumes a portion of the input buffer and sets next_token
    pub fn lex(&mut self) -> Status {
        // setup for lexing
        if let Some(next_token) = &self.next_token {
            self.token_stream.push(next_token.clone());
            self.next_token = None;
        }

        if let Some(next_lexeme) = &self.next_lexeme {
            self.lexeme_stream.push(next_lexeme.clone());
            self.next_lexeme = None;
        }

        if !(self.token_stream.len() == self.lexeme_stream.len()) {
            panic!("Token Stream and Lexeme Stream have different lengths");
        }

        let Status::Reading(char_read) = self.get_char() else {
            return Status::EOF;
        };

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
            } // end L paren
            ')' => {
                self.next_token = Some(Token::RParen);
                self.next_lexeme = Some(String::from(char_read));
            } // end R paren
            '{' => {
                self.next_token = Some(Token::LBrace);
                self.next_lexeme = Some(String::from(char_read));
            } // end L brace
            '}' => {
                self.next_token = Some(Token::RBrace);
                self.next_lexeme = Some(String::from(char_read));
            } // end R brace
            '[' => {
                self.next_token = Some(Token::LBracket);
                self.next_lexeme = Some(String::from(char_read));
            } // end L bracket
            ']' => {
                self.next_token = Some(Token::RBracket);
                self.next_lexeme = Some(String::from(char_read));
            } // end R bracket
            '+' => {
                self.next_token = Some(Token::OpAdd);
                self.next_lexeme = Some(String::from(char_read));
            } // end +
            '-' => {
                self.next_token = Some(Token::OpSub);
                self.next_lexeme = Some(String::from(char_read));
            } // end -
            '*' => {
                let Some(last_lexeme) = self.lexeme_stream.last() else {
                    self.next_token = Some(Token::OpMul);
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
                            break;
                        }
                    }
                } else {
                    self.next_token = Some(Token::OpDiv);
                    self.next_lexeme = Some(String::from(char_read));
                }
            } // end / 
            _ => {}
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
                self.current_word = String::new();
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
                self.current_word = String::new();
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


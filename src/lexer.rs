
use crate::token::Token;

use anyhow::Result;
use std::collections::HashMap;
use regex::Regex;
use std::io::BufReader;

struct Lexer {
    next_token: Option<Token>,
    next_lexeme: Option<String>,
    token_stream: HashMap<Token, String>,
    lexeme_stream: Vec<Token>,
    input_buffer: BufReader<String>,
    rules: HashMap<String, Regex>,
}

impl Lexer {
    /// Consumes a portion of the input buffer and sets next_token
    fn lex(&mut self) -> Result<()> {

        let x = self.input_buffer.get_ref();
        self.token_stream.insert(self.next_token.clone().unwrap(), self.next_lexeme.clone().unwrap());
        self.next_lexeme = None;
        self.next_token = None;
        


        Ok(())
    }
}

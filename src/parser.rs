use crate::ast::AstNode;
use crate::lexer::Lexer;
use crate::token::Token;

use std::sync::{Mutex, PoisonError};

lazy_static! {
    pub static ref AST: Mutex<AstNode> = Mutex::new(AstNode::new());
}

pub struct Parser {
    lexer: Lexer,
    pub logging: bool,
}

impl Parser {
    pub fn new(lexer: Lexer, logging: bool) -> Self {
        Self { lexer, logging }
    }

    fn log<T: std::fmt::Debug>(&mut self, item: T) {
        if self.logging {
            dbg!(item);
        }
    }

    fn parse_statement_list(&mut self) {
        self.log("Enter statement list");
        self.parse_statement();
        self.log("Exit statement list");
    }

    fn parse_statement(&mut self) {
        self.log("Enter statement");
        self.parse_expr();
        self.log("Exit statement");
    }

    fn parse_expr(&mut self) {
        self.log("Enter expr");
        self.parse_term();

        while self.lexer.next_token == Some(Token::OpAdd)
            || self.lexer.next_token == Some(Token::OpSub)
        {
            self.lexer.lex();
            self.parse_term();
        }
        self.log("Exit expr");
    }

    fn parse_term(&mut self) {
        self.log("Enter term");
        self.log("Exit term");
    }
}

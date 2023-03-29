
use crate::token::Token;

#[derive(Clone)]
pub struct AstNode {
    token: Option<Token>,
    lexeme: Option<String>,
    children: Vec<AstNode>,
}

impl AstNode {
    pub fn new() -> Self {
        Self {
            token: None,
            lexeme: None,
            children: vec![],
        }
    }
}


use crate::token::Token;

#[derive(Clone, Debug)]
pub struct AstNode {
    node_type: String,
    token: Option<Token>,
    lexeme: Option<String>,
    pub children: Vec<AstNode>,
}

impl AstNode {
    pub fn new<S: Into<String>>(node_type: S, token: Option<Token>, lexeme: Option<S>) -> Self {
        let mut new_lexeme: Option<String> = None;
        if let Some(s) = lexeme {
            new_lexeme = Some(s.into());
        }

        Self {
            node_type: node_type.into(),
            token,
            lexeme: new_lexeme,
            children: vec![],
        }
    }
}


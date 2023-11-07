use crate::token::Token;

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AstNode {
    pub node_type: NodeType,
    pub token: Option<Token>,
    pub lexeme: Option<String>,
    pub children: Vec<AstNode>,
}

impl AstNode {
    pub fn new(
        node_type: NodeType,
        token: Option<Token>,
        lexeme: Option<&str>,
    ) -> Self {
        let mut new_lexeme: Option<String> = None;
        if let Some(s) = lexeme {
            new_lexeme = Some(s.into());
        }

        Self {
            node_type,
            token,
            lexeme: new_lexeme,
            children: vec![],
        }
    }

    pub fn into_inner(self) -> Vec<AstNode> {
        return self.children;
    }
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Clone)]
pub enum NodeType {
    Error,
    Atom,
    ExpressionList,
    FunctionDefinition,
    ParameterList,
    Assignment,
    UnaryOperation,
    BinaryOperation,
}

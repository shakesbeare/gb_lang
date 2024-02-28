use crate::token::Token;

trait NodeType: std::fmt::Display {}

pub enum Node {
    Program(Program),
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let node = self.into_inner().as_ref();
        write!(f, "{}", node)
    }
}

impl Node {
    fn into_inner(self) -> Box<dyn NodeType> {
        match self {
            Node::Program(v) => Box::new(v),
        }
    }
}

enum Statement {

}

impl NodeType for Statement {}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "statement")
    }
}

enum Expression {

}

impl NodeType for Expression {}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "expression")
    }
}

pub struct Program {
    statements: Vec<Statement>,
}

impl NodeType for Program {}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut val = "".to_string();
        for item in self.statements.iter() {
            val.push_str(&item.to_string());
            val.push_str("\n");
        }
        write!(f, "{}", val)
    }
}

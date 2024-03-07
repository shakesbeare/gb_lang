mod node;

pub use node::*;

pub struct Program {
    pub statements: Vec<Node>,
}

#[derive(Debug)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug)]
pub enum Expression {
    Null,
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
}


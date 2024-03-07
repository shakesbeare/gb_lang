use std::rc::Rc;
use crate::token::Token;

use crate::ast::Expression;

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Rc<Expression>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Rc<Expression>,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Rc<Expression>,
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
}

impl Identifier {
    pub fn value(&self) -> &str {
        self.token.literal.as_str()
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Rc<Expression>,
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub operator: String,
    pub right: Rc<Expression>,
}

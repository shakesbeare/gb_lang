use crate::token::Token;
use std::rc::Rc;

use super::Expression;
use super::Node;
use super::Statement;

use super::IntoExpression;
use super::IntoNode;
use super::IntoStatement;

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Rc<Expression>,
}

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} = {}", self.token.literal, self.name, self.value)
    }
}

impl IntoNode for LetStatement {
    fn into_node(self) -> Node {
        Node::Statement(self.into_statement())
    }
}

impl IntoStatement for LetStatement {
    fn into_statement(self) -> Statement {
        Statement::LetStatement(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Rc<Expression>,
}

impl IntoStatement for ReturnStatement {
    fn into_statement(self) -> Statement {
        Statement::ReturnStatement(self)
    }
}

impl IntoNode for ReturnStatement {
    fn into_node(self) -> Node {
        Node::Statement(self.into_statement())
    }
}

impl std::fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.token.literal, self.return_value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Rc<Expression>,
}

impl IntoStatement for ExpressionStatement {
    fn into_statement(self) -> Statement {
        Statement::ExpressionStatement(self)
    }
}

impl IntoNode for ExpressionStatement {
    fn into_node(self) -> Node {
        Node::Statement(self.into_statement())
    }
}

impl std::fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Rc<Statement>>,
}

impl IntoStatement for BlockStatement {
    fn into_statement(self) -> Statement {
        Statement::BlockStatement(self)
    }
}

impl IntoNode for BlockStatement {
    fn into_node(self) -> Node {
        Node::Statement(self.into_statement())
    }
}

impl std::fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        write!(f, " }}")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteralStatement {
    pub token: Token,
    pub identifier: Identifier,
    pub literal: FunctionLiteral,
}

impl std::fmt::Display for FunctionLiteralStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}",
            self.identifier,
            self.literal
        )
    }
}

impl IntoStatement for FunctionLiteralStatement {
    fn into_statement(self) -> Statement {
        Statement::FunctionLiteralStatement(self)
    }
}

impl IntoNode for FunctionLiteralStatement {
    fn into_node(self) -> Node {
        Node::Statement(self.into_statement())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub token: Token,
}

impl IntoExpression for Identifier {
    fn into_expression(self) -> Expression {
        Expression::Identifier(self)
    }
}

impl IntoNode for Identifier {
    fn into_node(self) -> Node {
        Node::Expression(self.into_expression())
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl Identifier {
    pub fn value(&self) -> &str {
        self.token.literal.as_str()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntoExpression for IntegerLiteral {
    fn into_expression(self) -> Expression {
        Expression::IntegerLiteral(self)
    }
}

impl IntoNode for IntegerLiteral {
    fn into_node(self) -> Node {
        Node::Expression(self.into_expression())
    }
}

impl std::fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatLiteral {
    pub token: Token,
    pub value: f64,
}

impl IntoExpression for FloatLiteral {
    fn into_expression(self) -> Expression {
        Expression::FloatLiteral(self)
    }
}

impl IntoNode for FloatLiteral {
    fn into_node(self) -> Node {
        Node::Expression(self.into_expression())
    }
}

impl std::fmt::Display for FloatLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl IntoExpression for StringLiteral {
    fn into_expression(self) -> Expression {
        Expression::StringLiteral(self)
    }
}

impl IntoNode for StringLiteral {
    fn into_node(self) -> Node {
        Node::Expression(self.into_expression())
    }
}

impl std::fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Rc<Expression>,
}

impl IntoExpression for PrefixExpression {
    fn into_expression(self) -> Expression {
        Expression::PrefixExpression(self)
    }
}

impl IntoNode for PrefixExpression {
    fn into_node(self) -> Node {
        Node::Expression(self.into_expression())
    }
}

impl std::fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub operator: String,
    pub right: Rc<Expression>,
}

impl IntoExpression for InfixExpression {
    fn into_expression(self) -> Expression {
        Expression::InfixExpression(self)
    }
}

impl IntoNode for InfixExpression {
    fn into_node(self) -> Node {
        Node::Expression(self.into_expression())
    }
}

impl std::fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl IntoExpression for BooleanLiteral {
    fn into_expression(self) -> Expression {
        Expression::BooleanLiteral(self)
    }
}

impl IntoNode for BooleanLiteral {
    fn into_node(self) -> Node {
        Node::Expression(self.into_expression())
    }
}

impl std::fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Alternative {
    IfExpression(Rc<Expression>),
    BlockStatement(BlockStatement),
    None,
}

impl Alternative {
    pub fn is_some(&self) -> bool {
        match self {
            Alternative::IfExpression(_) => true,
            Alternative::BlockStatement(_) => true,
            Alternative::None => false,
        }
    }

    pub fn is_none(&self) -> bool {
        !self.is_some()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Rc<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Alternative,
}

impl IntoExpression for IfExpression {
    fn into_expression(self) -> Expression {
        Expression::IfExpression(self)
    }
}

impl IntoNode for IfExpression {
    fn into_node(self) -> Node {
        Node::Expression(self.into_expression())
    }
}

impl std::fmt::Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.alternative {
            Alternative::IfExpression(c) => {
                write!(f, "if {} {} else {}", self.condition, self.consequence, c,)
            }
            Alternative::BlockStatement(t) => {
                write!(f, "if {} {} else {}", self.condition, self.consequence, t,)
            }
            Alternative::None => {
                write!(f, "if {} {}", self.condition, self.consequence)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl IntoExpression for FunctionLiteral {
    fn into_expression(self) -> Expression {
        Expression::FunctionLiteral(self)
    }
}

impl IntoNode for FunctionLiteral {
    fn into_node(self) -> Node {
        Node::Expression(self.into_expression())
    }
}

impl std::fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({}) {}",
            self.token.literal,
            self.parameters
                .iter()
                .map(|p| p.value())
                .collect::<Vec<&str>>()
                .join(", "),
            self.body
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Rc<Expression>,
    pub arguments: Vec<Expression>,
}

impl IntoExpression for CallExpression {
    fn into_expression(self) -> Expression {
        Expression::CallExpression(self)
    }
}

impl IntoNode for CallExpression {
    fn into_node(self) -> Node {
        Node::Expression(self.into_expression())
    }
}

impl std::fmt::Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            self.arguments
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

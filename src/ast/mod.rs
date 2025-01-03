mod node;

pub use node::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
    Empty,
}

pub trait IntoNode {
    fn into_node(self) -> Node;
}

impl Node {
    /// Get the Program from the Node, panicking if it's not a Program
    pub fn into_program(self) -> Program {
        match self {
            Node::Program(p) => p,
            _ => panic!("Expected Program, got {:?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Node>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
    FunctionLiteralStatement(FunctionLiteralStatement),
}

impl IntoNode for Statement {
    fn into_node(self) -> Node {
        Node::Statement(self)
    }
}

impl Statement {
    pub fn is_return_stmt(&self) -> bool {
        matches!(self, Statement::ReturnStatement(_))
    }
}

pub trait IntoStatement {
    fn into_statement(self) -> Statement;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
    StringLiteral(StringLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    BooleanLiteral(BooleanLiteral),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    WhileExpression(WhileExpression),
}

impl IntoNode for Expression {
    fn into_node(self) -> Node {
        Node::Expression(self)
    }
}

pub trait IntoExpression {
    fn into_expression(self) -> Expression;
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Program(p) => write!(f, "{}", p),
            Node::Statement(s) => write!(f, "{}", s),
            Node::Expression(e) => write!(f, "{}", e),
            Node::Empty => write!(f, ""),
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(s) => write!(f, "{}", s),
            Statement::ReturnStatement(s) => write!(f, "{}", s),
            Statement::ExpressionStatement(s) => write!(f, "{}", s),
            Statement::BlockStatement(s) => write!(f, "{}", s),
            Statement::FunctionLiteralStatement(s) => write!(f, "{}", s),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(i) => write!(f, "{}", i),
            Expression::IntegerLiteral(i) => write!(f, "{}", i),
            Expression::FloatLiteral(fl) => write!(f, "{}", fl),
            Expression::StringLiteral(sl) => write!(f, "{}", sl),
            Expression::PrefixExpression(p) => write!(f, "{}", p),
            Expression::InfixExpression(i) => write!(f, "{}", i),
            Expression::BooleanLiteral(b) => write!(f, "{}", b),
            Expression::IfExpression(i) => write!(f, "{}", i),
            Expression::FunctionLiteral(f_) => write!(f, "{}", f_),
            Expression::CallExpression(c) => write!(f, "{}", c),
            Expression::WhileExpression(we) => write!(f, "{}", we),
        }
    }
}

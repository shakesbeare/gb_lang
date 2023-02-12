use crate::gb_type::GbType;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    UnaryMinus(Box<Expr>),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
    CompoundExpr(Vec<Expr>),
    Return(Box<Expr>),
    If {
        condition: Box<Expr>,
        expr: Box<Expr>,
        els: Option<Box<Expr>>,
    },
    While {
        condition: Box<Expr>,
        expr: Box<Expr>,
    },
    FunctionCall {
        identifier: Box<Expr>,
        args: Vec<Expr>,
    },
    FunctionDefinition {
        arg_types: Vec<GbType>,
        arg_names: Vec<String>,
        body: Box<Expr>,
    },

    Print,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Assign,
    LessThan,
    GreaterThan,
}

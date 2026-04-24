use gbc_lex::Token;
use gbc_shared::Span;

#[derive(Debug, Clone)]
pub struct Expr {
    span: Span,
    kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(Token),
    Unary(UnOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Deref,
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub enum BinOp {}

#[derive(Debug, Clone)]
pub struct Stmt {
    span: Span,
    kind: StmtKind,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// An expression
    Expr(Box<Expr>),
    /// An expression with a trailing semicolon
    ExprTerminated(Box<Expr>),
}

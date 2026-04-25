use gbc_shared::Span;

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    Expr(Expr),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    Identifier(Identifier),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier {
    pub span: Span,
}

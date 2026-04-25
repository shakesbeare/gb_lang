use gbc_shared::{Span, macros::AstNode as AstNodeDerive};
use derive_tools::Unwrap;

pub trait AstNode {
    fn span(&self) -> &Span;
    fn render(&self, input: &str) -> String {
        String::from(&input[self.span()])
    }
}

#[derive(AstNodeDerive, Debug, PartialEq, Eq)]
pub struct Program {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(AstNodeDerive, Debug, PartialEq, Eq)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug, PartialEq, Eq, Unwrap)]
#[unwrap(ref)]
pub enum StmtKind {
    Expr(Expr),
    ExprTerm(Expr),
}

#[derive(AstNodeDerive, Debug, PartialEq, Eq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Eq, Unwrap)]
#[unwrap(ref)]
pub enum ExprKind {
    Ident(Ident),
}

#[derive(AstNodeDerive, Debug, PartialEq, Eq)]
pub struct Ident {
    pub span: Span,
}

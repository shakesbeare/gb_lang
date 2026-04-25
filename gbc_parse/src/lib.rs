extern crate self as gbc_parse;

mod parse_tree;
mod syntax_error;
#[cfg(test)]
mod tests;

use std::sync::Arc;

use gbc_lex::{lexer::Lexer, Token, TokenKind};
use gbc_shared::Span;

use crate::{
    parse_tree::{Expr, ExprKind, Ident, Program, Stmt, StmtKind},
    syntax_error::{SyntaxError, SyntaxErrorKind},
};

pub struct Parser<'input> {
    lexer: Lexer<'input>,
    end_of_file_reached: bool,
}

impl<'input> Parser<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            lexer: Lexer::new(input),
            end_of_file_reached: false,
        }
    }

    fn expect_peek(&mut self, kind: TokenKind) -> Result<Arc<Token>, SyntaxError> {
        let Some(tok) = self.peek_token() else {
            let span = Span::new(self.lexer.pos().unwrap_or(0), self.lexer.peek_pos());
            return Err(SyntaxError::new(span, SyntaxErrorKind::UnexpectedEndOfFile));
        };

        if tok.kind == kind {
            Ok(Arc::clone(&tok))
        } else {
            Err(SyntaxError::new(
                tok.span.clone(),
                SyntaxErrorKind::UnexpectedToken(tok),
            ))
        }
    }

    fn guess_peek(&mut self, kind: TokenKind) -> bool {
        if let Some(tok) = self.peek_token() && tok.kind == kind {
            true
        } else {
            false
        }
    }

    fn next_token(&mut self) -> Option<Arc<Token>> {
        let tok = self.lexer.next();
        if tok.is_none() {
            self.end_of_file_reached = true
        }

        tok
    }

    #[allow(unused)]
    fn peek_token(&mut self) -> Option<Arc<Token>> {
        self.lexer.peek().cloned()
    }

    pub fn parse(&mut self) -> Program {
        self.parse_program()
    }

    fn parse_program(&mut self) -> Program {
        let mut out = vec![];
        let mut syntax_errors = vec![];

        while self.lexer.peek().is_some() {
            match self.parse_stmt() {
                Ok(stmt) => out.push(stmt),
                Err(err) => syntax_errors.push(err),
            }
        }

        Program {
            span: Span::new(0, self.lexer.pos().unwrap_or(0)),
            stmts: out,
        }
    }

    fn parse_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        _ = self.expect_peek(TokenKind::Ident)?;

        let expr = self.parse_expr()?;
        let mut span = expr.span.clone();
        let kind = if self.guess_peek(TokenKind::Semicolon) {
            _ = self.next_token(); // pass over the semicolon
            span.extend(1);
            StmtKind::ExprTerm(expr)
        } else {
            StmtKind::Expr(expr)
        };

        Ok(Stmt {
            span,
            kind,
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, SyntaxError> {
        let token = self.next_token().unwrap();
        Ok(Expr {
            span: token.span.clone(),
            kind: ExprKind::Ident(Ident {
                span: token.span.clone(),
            }),
        })
    }
}

pub trait GbParser<'input> {
    fn gb_parser(&'input self) -> Parser<'input>;
}

impl<'input, T> GbParser<'input> for T
where
    T: AsRef<str> + 'input,
{
    fn gb_parser(&'input self) -> Parser<'input> {
        let input = self.as_ref();
        Parser::new(input)
    }
}

use std::io::Read;
use crate::token::HasKind;

use crate::{
    ast::{AstNode, NodeType},
    lexer::Lexer,
    token::{Token, TokenKind},
};

pub struct Parser<T: Read> {
    pub lexer: Lexer<T>,
    pub verbose: bool,
}

impl<R: Read> Parser<R> {
    pub fn new(lexer: Lexer<R>, verbose: bool) -> Self {
        Self { lexer, verbose }
    }

    // Useful for debugging, set verbose to true
    fn print<T: std::fmt::Debug>(&self, item: T) {
        if self.verbose {
            println!("{:?}", item);
        }
    }

    /// Parses the entire program and returns the abstract syntax tree formed of AstNode instances.
    /// This also saturates the fields of the internal lexer.
    pub fn parse(&mut self) -> AstNode {
        self.print("start program");

        // Grab the first token from the lexer and build the expression list
        // Must grab the token before parsing begins so that self.lexer.next_token is
        // not none
        let status = self.lexer.lex();
        self.print(status);
        let ast = self.expression_list();

        if !self.lexer.next_token.has_kind(TokenKind::Eof) {
            self.syntax_error(self.lexer.next_token.clone().unwrap());
        }

        self.print("end program");
        return ast;
    }

    fn expression_list(&mut self) -> AstNode {
        self.print("start expr_list");

        // Create the new node
        let mut expr_list = AstNode::new(NodeType::ExpressionList, None, None);

        // While there are expressions left to parse,
        // Parse them
        // We have to make sure that we reach the end of input
        // By passing over any extra EOL tokens and any potential None tokens
        // Before we reach the EOF token.
        while !self.lexer.next_token.has_kind(TokenKind::Eof)
            && !self.lexer.next_token.has_kind(TokenKind::RBrace)
        {
            expr_list.children.push(self.add_sub());

            if !self.lexer.next_token.has_kind(TokenKind::Semicolon) {
                self.syntax_error(self.lexer.next_token.clone().unwrap());
                break;
            } else {
                let status = self.lexer.lex();
                self.print(status);
            }

            while self.lexer.next_token.has_kind(TokenKind::Eol)
                || self.lexer.next_token.is_none()
            {
                let status = self.lexer.lex();
                self.print(status);
            }
        }
        self.print("end expr_list");
        return expr_list;
    }

    fn assignment(&mut self) -> AstNode {
        self.print("start assignment");

        // get left identifier
        let left = self.atom();

        // get = sign
        if !self.lexer.next_token.has_kind(TokenKind::OpAssign) {
            self.syntax_error(self.lexer.next_token.clone().unwrap());
        } else {
            let status = self.lexer.lex();
            self.print(status);
        };

        // parse rhs
        let right = self.add_sub();

        let mut ast = AstNode::new(NodeType::Assignment, None, None);
        ast.children.append(&mut vec![right, left]);

        return ast;
    }

    fn param_list(&mut self) -> AstNode {
        self.print("start param list");
        let mut params = vec![];
        while !self.lexer.next_token.has_kind(TokenKind::RParen) {
            params.push(self.atom());

            if self.lexer.next_token.has_kind(TokenKind::RParen) {
                break;
            } else if self.lexer.next_token.has_kind(TokenKind::Comma) {
                self.lexer.lex();
            }
        }

        let mut ast = AstNode::new(NodeType::ParameterList, None, None);
        ast.children.append(&mut params);

        self.print("end param list");
        return ast;
    }

    fn func_definition(&mut self) -> AstNode {
        self.print("start function definition");

        // parse the parameter list
        if !self.lexer.next_token.has_kind(TokenKind::LParen) {
            self.syntax_error(self.lexer.next_token.clone().unwrap());
            return AstNode::new(NodeType::Error, None, None);
        } else {
            let status = self.lexer.lex();
            self.print(status);
        }

        // parse the parameter list
        let param_list = self.param_list();

        // pass over the r paren
        while !self.lexer.next_token.has_kind(TokenKind::RParen) {
            self.lexer.lex();

            if self.lexer.next_token.has_kind(TokenKind::Eof) {
                self.syntax_error(self.lexer.next_token.clone().unwrap());
                return AstNode::new(NodeType::Error, None, None);
            }
        }

        self.lexer.lex();

        // pass over the l brace
        // we can guarantee this is here by the calling context
        self.lexer.lex();

        // parse the expression list
        let exp_list = self.expression_list();

        // pass over the r brace
        while !self.lexer.next_token.has_kind(TokenKind::RBrace) {
            self.lexer.lex();

            if self.lexer.next_token.has_kind(TokenKind::Eof) {
                self.syntax_error(self.lexer.next_token.clone().unwrap());
                return AstNode::new(NodeType::Error, None, None);
            }
        }

        let mut ast = AstNode::new(NodeType::FunctionDefinition, None, None);
        ast.children.append(&mut vec![exp_list, param_list]);

        self.print("end function definition");
        return ast;
    }

    fn add_sub(&mut self) -> AstNode {
        self.print("start add_sub");
        let mut left_child = self.mul_div();

        // if there is any operators
        // handle them and search for a right operand
        while self.lexer.next_token.has_kind(TokenKind::OpAdd)
            || self.lexer.next_token.has_kind(TokenKind::OpSub)
        {
            let op_tok = self.lexer.next_token.clone();
            let status = self.lexer.lex();
            self.print(status);
            let right_child = self.mul_div();
            let mut bin_op =
                AstNode::new(NodeType::BinaryOperation, op_tok, None);
            bin_op.children.append(&mut vec![left_child, right_child]);

            left_child = bin_op;
        }

        self.print("end add_sub");
        return left_child;
    }

    fn mul_div(&mut self) -> AstNode {
        self.print("start mul_div");
        let mut left_child = self.exponentiation();

        // if there is any operators
        // handle them and search for a right operand
        while self.lexer.next_token.has_kind(TokenKind::OpMul)
            || self.lexer.next_token.has_kind(TokenKind::OpDiv)
        {
            let op_tok = self.lexer.next_token.clone();
            let status = self.lexer.lex();
            self.print(status);
            let right_child = self.exponentiation();
            let mut bin_op =
                AstNode::new(NodeType::BinaryOperation, op_tok, None);
            bin_op.children.append(&mut vec![left_child, right_child]);

            left_child = bin_op;
        }

        self.print("end mul_div");
        return left_child;
    }

    fn exponentiation(&mut self) -> AstNode {
        self.print("start exponentiation");
        let mut left_child = self.unary_op();

        // if there is any operators
        // handle them and search for a right operand
        while self.lexer.next_token.has_kind(TokenKind::OpExp) {
            let op_tok = self.lexer.next_token.clone();
            let status = self.lexer.lex();
            self.print(status);
            let right_child = self.unary_op();
            let mut bin_op =
                AstNode::new(NodeType::BinaryOperation, op_tok, None);
            // exponentiation is right associative
            bin_op.children.append(&mut vec![right_child, left_child]);

            left_child = bin_op;
        }

        self.print("end exponentiation");
        return left_child;
    }

    fn unary_op(&mut self) -> AstNode {
        self.print("start unary operation");
        let mut node = self.get_unary_operator();

        if node.node_type == NodeType::UnaryOperation {
            let child = self.unary_op();
            node.children.push(child);
        }

        return node;
    }

    fn get_unary_operator(&mut self) -> AstNode {
        let tok = self.lexer.next_token.clone();

        let Some(token) = tok.clone() else {
            panic!("Not a real token");
        };

        match token.kind {
            _t if [TokenKind::OpBang, TokenKind::OpAdd, TokenKind::OpSub]
                .contains(&token.kind) =>
            {
                self.lexer.lex();
                return AstNode::new(NodeType::UnaryOperation, tok, None);
            }
            _ => {
                return self.atom();
            }
        }
    }

    fn atom(&mut self) -> AstNode {
        self.print("start atom");

        let Some(token) = &self.lexer.next_token.clone() else {
            dbg!(&self.lexer.token_stream);
            dbg!(&self.lexer.next_token);
            unreachable!()
        };

        let mut ast = AstNode::new(NodeType::Error, None, None);

        // If the token is an atom, create a new node for it and return it
        // If the token is a LParen, handle it by calling back up to parse_expr (Note: the
        // corresponding RParen is checked for inside the LParen match arm)
        // Otherwise, there is a syntax error
        match token.kind {
            TokenKind::IntLiteral
            | TokenKind::FloatLiteral
            | TokenKind::Identifier
            | TokenKind::Boolean => {
                self.print(format!(
                    "Found atomic value: {:?}:{}",
                    token, token.literal
                ));
                ast = AstNode::new(
                    NodeType::Atom,
                    Some(token.clone()),
                    Some(&token.literal),
                );
            }
            TokenKind::LParen => {
                self.print("BEGIN PAREN");
                // Pass over the paren
                let status = self.lexer.lex();
                self.print(status);

                // Parse the inner expressions
                ast = self.add_sub();

                // check for closing paren
                if !self.lexer.next_token.has_kind(TokenKind::RParen) {
                    // malformed expression, syntax error
                    self.syntax_error(self.lexer.next_token.clone().unwrap());
                }

                self.print("END PAREN");
            }
            TokenKind::Keyword => match token.literal.as_str() {
                "let" => {
                    self.lexer.lex();
                    ast = self.assignment();
                }
                "fn" => {
                    self.lexer.lex();
                    ast = self.func_definition();
                }
                _ => self.syntax_error(token.clone()),
            },
            x => self.syntax_error(token.clone()),
        };

        let status = self.lexer.lex();
        self.print(status);

        self.print("end atom");
        return ast;
    }

    fn syntax_error(&self, token: Token) {
        println!(
            "Encountered a syntax error at {}, {}: Unexpected {:?}",
            self.lexer.line, self.lexer.col, token
        );
    }
}

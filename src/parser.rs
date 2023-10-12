use std::io::Read;

use crate::ast::AstNode;
use crate::lexer::Lexer;
use crate::token::Token;

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

        self.print("end program");
        return ast;
    }

    fn expression_list(&mut self) -> AstNode {
        self.print("start expr_list");

        // Create the new node
        let mut expr_list = AstNode::new("Expression list", None, None);

        // While there are expressions left to parse,
        // Parse them
        // We have to make sure that we reach the end of input
        // By passing over any extra EOL tokens and any potential None tokens
        // Before we reach the EOF token.
        while self.lexer.next_token != Some(Token::Eof) {
            expr_list.children.push(self.add_sub());

            if self.lexer.next_token != Some(Token::Semicolon) {
                self.syntax_error(self.lexer.next_token.clone().unwrap());
                break;
            } else {
                let status = self.lexer.lex();
                self.print(status);
            }

            while self.lexer.next_token == Some(Token::Eol)
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
        if self.lexer.next_token != Some(Token::OpAssign) {
            self.syntax_error(self.lexer.next_token.clone().unwrap());
        } else {
            let status = self.lexer.lex();
            self.print(status);
        };

        // parse rhs
        let right = self.add_sub();

        let mut ast = AstNode::new("Assignment", None, None);
        ast.children.append(&mut vec![right, left]);

        return ast;
    }

    fn add_sub(&mut self) -> AstNode {
        self.print("start expression");
        let mut left_child = self.mul_div();

        // if there is any operators
        // handle them and search for a right operand
        while self.lexer.next_token == Some(Token::OpAdd)
            || self.lexer.next_token == Some(Token::OpSub)
        {
            let op_tok = self.lexer.next_token.clone();
            let status = self.lexer.lex();
            self.print(status);
            let right_child = self.mul_div();
            let mut bin_op = AstNode::new("Binary Operation", op_tok, None);
            bin_op.children.append(&mut vec![left_child, right_child]);

            left_child = bin_op;
        }

        self.print("end expression");
        return left_child;
    }

    fn mul_div(&mut self) -> AstNode {
        self.print("start term");
        let mut left_child = self.exponentiation();

        // if there is any operators
        // handle them and search for a right operand
        while self.lexer.next_token == Some(Token::OpMul)
            || self.lexer.next_token == Some(Token::OpDiv)
        {
            let op_tok = self.lexer.next_token.clone();
            let status = self.lexer.lex();
            self.print(status);
            let right_child = self.exponentiation();
            let mut bin_op = AstNode::new("Binary Operation", op_tok, None);
            bin_op.children.append(&mut vec![left_child, right_child]);

            left_child = bin_op;
        }

        self.print("end term");
        return left_child;
    }

    fn exponentiation(&mut self) -> AstNode {
        self.print("start exponentiation");
        let mut left_child = self.atom();

        // if there is any operators
        // handle them and search for a right operand
        while self.lexer.next_token == Some(Token::OpExp) {
            let op_tok = self.lexer.next_token.clone();
            let status = self.lexer.lex();
            self.print(status);
            let right_child = self.atom();
            let mut bin_op = AstNode::new("Binary Operation", op_tok, None);
            // exponetiation is right associative
            bin_op.children.append(&mut vec![right_child, left_child]);

            left_child = bin_op;
        }

        self.print("end exponentiation");
        return left_child;
    }

    fn atom(&mut self) -> AstNode {
        self.print("start factor");

        let Some(token) = &self.lexer.next_token.clone() else {
            dbg!(&self.lexer.token_stream);
            dbg!(&self.lexer.next_token);
            unreachable!()
        };

        let Some(lexeme) = &self.lexer.next_lexeme.clone() else {
            dbg!(&self.lexer.lexeme_stream);
            dbg!(&self.lexer.next_lexeme);
            unreachable!();
        };

        let mut ast = AstNode::new("ERROR", None, None);

        // If the token is an atom, create a new node for it and return it
        // If the token is a LParen, handle it by calling back up to parse_expr (Note: the
        // corresponding RParen is checked for inside the LParen match arm)
        // Otherwise, there is a syntax error
        match token {
            Token::IntLiteral
            | Token::FloatLiteral
            | Token::Identifier
            | Token::Boolean => {
                self.print(format!("Found atomic value: {:?}:{}", token, lexeme));
                ast = AstNode::new("Atom", Some(token.clone()), Some(lexeme));
            }
            Token::LParen => {
                self.print("BEGIN PAREN");
                // Pass over the paren
                let status = self.lexer.lex();
                self.print(status);

                // Parse the inner expressions
                ast = self.add_sub();

                // check for closing paren
                if self.lexer.next_token != Some(Token::RParen) {
                    // malformed expression, syntax error
                    self.syntax_error(self.lexer.next_token.clone().unwrap());
                }

                self.print("END PAREN");
            }
            Token::Keyword => match lexeme.as_str() {
                "let" => {
                    self.lexer.lex();
                    ast = self.assignment();
                }
                _ => self.syntax_error(token.clone()),
            },
            x => self.syntax_error(x.clone()),
        };

        let status = self.lexer.lex();
        self.print(status);

        self.print("end factor");
        return ast;
    }

    fn syntax_error(&self, token: Token) {
        println!(
            "Encountered a syntax error at {}, {}: Unexpected {:?}",
            self.lexer.line, self.lexer.col, token
        );
    }
}

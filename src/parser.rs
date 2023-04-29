use crate::ast::AstNode;
use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser {
    pub lexer: Lexer,
    pub verbose: bool,
}

impl Parser {
    pub fn new(lexer: Lexer, verbose: bool) -> Self {
        Self {
            lexer,
            verbose,
        }
    }

    // Useful for debugging, set verbose to true
    fn print<T: std::fmt::Debug>(&self, item: T) {
        if self.verbose {
            println!("{:?}", item);
        }
    }

    pub fn parse_program(&mut self) -> AstNode {
        self.print("start program");

        // Grab the first token from the lexer and build the expression list
        // Must grab the token before parsing begins so that self.lexer.next_token is
        // not none
        self.lexer.lex();
        let ast = self.parse_expr_list();

        self.print("end program");
        return ast;
    }

    fn parse_expr_list(&mut self) -> AstNode {
        self.print("    start expr_list");

        // Create the new node
        let mut expr_list = AstNode::new("Expression list", None, None);

        // While there are expressions left to parse,
        // Parse them
        // We have to make sure that we reach the end of input
        // By passing over any extra EOL tokens and any potential None tokens
        // Before we reach the EOF token.
        while self.lexer.next_token != Some(Token::EOF) {
            expr_list.children.push(self.parse_expr());

            if self.lexer.next_token != Some(Token::Semicolon) {
                self.syntax_error(self.lexer.next_token.clone().unwrap());
                break;
            } else {
                self.lexer.lex();
            }

            while self.lexer.next_token == Some(Token::EOL)
                || self.lexer.next_token == None
            {
                self.lexer.lex();
                dbg!(&self.lexer.next_token);
            }
        }

        self.print("    end expr_list");
        return expr_list;
    }

    fn parse_expr(&mut self) -> AstNode {
        self.print("        start expression");
        let mut left_child = self.parse_term();

        // if there is any operators
        // handle them and search for a right operand
        while self.lexer.next_token == Some(Token::OpAdd)
            || self.lexer.next_token == Some(Token::OpSub)
        {
            let op_tok = self.lexer.next_token.clone();
            self.lexer.lex();
            let right_child = self.parse_term();
            let mut bin_op = AstNode::new("Binary Operation", op_tok, None);
            bin_op.children.append(&mut vec![left_child, right_child]);

            left_child = bin_op;
        }

        self.print("        end expression");
        return left_child;
    }

    fn parse_term(&mut self) -> AstNode {
        self.print("            start term");
        let mut left_child = self.parse_exponentiation();

        // if there is any operators
        // handle them and search for a right operand
        while self.lexer.next_token == Some(Token::OpMul)
            || self.lexer.next_token == Some(Token::OpDiv)
        {
            let op_tok = self.lexer.next_token.clone();
            self.lexer.lex();
            let right_child = self.parse_exponentiation();
            let mut bin_op = AstNode::new("Binary Operation", op_tok, None);
            bin_op.children.append(&mut vec![left_child, right_child]);

            left_child = bin_op;
        }

        self.print("            end term");
        return left_child;
    }

    fn parse_exponentiation(&mut self) -> AstNode {
        self.print("                start exponentiation");
        let mut left_child = self.parse_factor();

        // if there is any operators
        // handle them and search for a right operand
        while self.lexer.next_token == Some(Token::OpExp) {
            let op_tok = self.lexer.next_token.clone();
            self.lexer.lex();
            let right_child = self.parse_factor();
            let mut bin_op = AstNode::new("Binary Operation", op_tok, None);
            bin_op.children.append(&mut vec![right_child, left_child]); // exponetiation is right

            left_child = bin_op;
        }

        self.print("                end exponentiation");
        return left_child;
    }

    fn parse_factor(&mut self) -> AstNode {
        self.print("                    start factor");

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
            Token::IntLiteral | Token::FloatLiteral | Token::Identifier => {
                self.print(format!(
                    "                        Found atomic value: {:?}:{}",
                    token, lexeme
                ));
                ast = AstNode::new("Atom", Some(token.clone()), Some(&lexeme));
            }
            Token::LParen => {
                self.print("BEGIN PAREN");
                // Pass over the paren
                self.lexer.lex();

                // Parse the inner expressions
                ast = self.parse_expr();

                // check for closing paren
                if self.lexer.next_token != Some(Token::RParen) {
                    // malformed expression, syntax error
                    self.syntax_error(self.lexer.next_token.clone().unwrap());
                }

                self.print("END PAREN");
            }
            x => self.syntax_error(x.clone()),
        };

        self.lexer.lex();

        self.print("                    end factor");
        return ast;
    }

    fn syntax_error(&self, token: Token) {
        println!(
            "Encountered a syntax error at {}, {}: Unexpected {:?}",
            self.lexer.line, self.lexer.col, token
        );
    }
}

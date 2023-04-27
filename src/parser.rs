use crate::ast::AstNode;
use crate::lexer::{Lexer, Status};
use crate::token::Token;

pub struct Parser {
    pub lexer: Lexer,
    pub logging: bool,
}

impl Parser {
    pub fn new(lexer: Lexer, logging: bool) -> Self {
        Self { lexer, logging }
    }

    fn log<T: std::fmt::Debug>(&self, item: T) {
        if self.logging {
            println!("{:?}", item);
        }
    }

    pub fn parse_program(&mut self) -> AstNode {
        self.log("start program");
        self.lexer.lex();

        let mut ast = AstNode::new("Program", None, None);
        ast.children.push(self.parse_expr_list());

        self.log("end program");

        return ast;
    }

    fn parse_expr_list(&mut self) -> AstNode {
        self.log("    start expr_list");
        let mut expr_list = AstNode::new("Expression list", None, None);

        while self.lexer.next_token != Some(Token::EOF) {
            if self.lexer.next_token == Some(Token::EOL) {
                self.lexer.lex();
                if self.lexer.next_token == Some(Token::EOF) || self.lexer.next_token == None {
                    break;
                }
            }
            expr_list.children.push(self.parse_expr());
        }

        self.log("    end expr_list");
        return expr_list;
    }

    fn parse_expr(&mut self) -> AstNode {
        self.log("        start expression");
        let child = self.parse_term();
        let mut children: Vec<AstNode> = vec![];
        children.push(child);

        while self.lexer.next_token == Some(Token::OpAdd) || self.lexer.next_token == Some(Token::OpSub) {
            let op_tok = self.lexer.next_token.clone();
            self.lexer.lex();
            let child = self.parse_term();
            let mut bin_op = AstNode::new("Binary Operation", op_tok, None);
            bin_op.children.append(&mut vec![children.pop().unwrap(), child]);
            children.push(bin_op);

        }

        let mut expr = AstNode::new("Expression", None, None);
        expr.children.append(&mut children);

        self.log("        end expression");
        return expr;
    }

    fn parse_term(&mut self) -> AstNode {
        self.log("            start term");
        let child = self.parse_factor();
        let mut children: Vec<AstNode> = vec![];
        children.push(child);

        while self.lexer.next_token == Some(Token::OpMul) || self.lexer.next_token == Some(Token::OpDiv) {
            let op_tok = self.lexer.next_token.clone();
            self.lexer.lex();
            let child = self.parse_factor();
            let mut bin_op = AstNode::new("Binary Operation", op_tok, None);
            bin_op.children.append(&mut vec![children.pop().unwrap(), child]);
            children.push(bin_op);

        }

        let mut term = AstNode::new("Term", None, None);
        term.children.append(&mut children);

        self.log("            end term");
        return term;
    }

    fn parse_factor(&mut self) -> AstNode {
        self.log("                start factor");
        

        let Some(tok) = &self.lexer.next_token.clone() else {
            unreachable!()
        };

        let Some(lexeme) = &self.lexer.next_lexeme.clone() else {
            unreachable!();
        };

        match tok {
            Token::IntLiteral => (),
            Token::FloatLiteral => (),
            Token::StringLiteral => (),
            Token::Identifier => (),
            Token::EOL => (),
            Token::EOF => (),
            x => self.syntax_error(x.clone()),
        };

        self.lexer.lex();

        self.log("                end factor");
        return AstNode::new("Atom", Some(tok.clone()), Some(&lexeme));
    }

    fn syntax_error(&self, token: Token) {
        println!("Encountered a syntax error at {}, {}: Unexpected {:?}", self.lexer.line, self.lexer.col, token);
    }
}

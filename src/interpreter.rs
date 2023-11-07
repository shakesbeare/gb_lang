// Node children should be evaluated left to right

use crate::{
    ast::{AstNode, NodeType},
    gb_type::{gb_pow, gb_type_of, variant_eq, GbType},
    lexer::Lexer,
    parser::Parser,
    scope::Scope,
};

use crate::token::Token;

use std::{fs::File, io::Read};

pub struct Interpreter<T: Read> {
    pub parser: Parser<T>,
}

impl From<File> for Interpreter<File> {
    fn from(file: File) -> Interpreter<File> {
        let lexer = Lexer::from(file);
        let parser = Parser::new(lexer, false);
        Interpreter {
            parser,
        }
    }
}

impl From<&'static [u8]> for Interpreter<&[u8]> {
    fn from(value: &'static [u8]) -> Interpreter<&[u8]> {
        let lexer = Lexer::from(value);
        let parser = Parser::new(lexer, false);
        Interpreter {
            parser,
        }
    }
}

impl Interpreter<File> {
    pub fn open_file<P: std::convert::AsRef<std::path::Path>>(
        filename: P,
    ) -> Self {
        let file = std::fs::File::open(filename);
        let Ok(file) = file else {
            panic!("Could not open file");
        };

        return Interpreter::from(file);
    }
}

impl<T: Read> Interpreter<T> {
    /// Interprets the text contained within the lexer
    pub fn interpret_inner(&mut self) -> GbType {
        // parse input
        let ast = self.parser.parse();
        // evaluate the abstract syntax tree
        return self.evaluate(ast, &mut Scope::init());
    }

    pub fn interpret(&mut self, input: T) -> GbType {
        self.parser.lexer.new_input(input);
        return self.interpret_inner();
    }

    fn error<S: Into<String>>(&self, msg: S) {
        println!("{}", msg.into());
    }

    fn evaluate(&mut self, ast: AstNode, current_scope: &mut Scope) -> GbType {
        match ast.node_type {
            NodeType::Error => GbType::Error,
            NodeType::Atom => self.evaluate_atom(ast, current_scope),
            NodeType::ExpressionList => {
                let mut last_res = GbType::Error;
                for node in ast.children {
                    last_res = self.evaluate(node, current_scope);
                }
                last_res
            }
            NodeType::FunctionDefinition => todo!(),
            NodeType::ParameterList => todo!(),
            NodeType::Assignment => todo!(),
            NodeType::UnaryOperation => {
                let Some(op_tok) = ast.token else {
                    self.error("Encountered unary operation with no operator");
                    return GbType::Error;
                };

                let expression = self.evaluate(
                    ast.children.first().unwrap().clone(),
                    current_scope,
                );

                match op_tok {
                    Token::OpAdd => expression,
                    Token::OpSub => GbType::Integer(0) - expression,
                    Token::OpBang => !expression,
                    _ => {
                        self.error("Encountered unary operation, but operator token was not a unary operator");
                        return GbType::Error;
                    }
                }
            }
            NodeType::BinaryOperation => {
                let Some(op_tok) = ast.token else {
                    self.error("Encountered binary operation with no operator");
                    return GbType::Error;
                };

                let left = self.evaluate(
                    ast.children.first().unwrap().clone(),
                    current_scope,
                );
                let right = self.evaluate(
                    ast.children.last().unwrap().clone(),
                    current_scope,
                );

                match op_tok {
                    Token::OpAdd => left + right,
                    Token::OpSub => left - right,
                    Token::OpMul => left * right,
                    Token::OpDiv => left / right,
                    Token::OpExp => gb_pow(right, left), // exponentiation is right associative
                    Token::OpAssign => {
                        let rhs = self.evaluate(
                            ast.children.first().unwrap().clone(),
                            current_scope,
                        );
                        let lhs = ast
                            .children
                            .last()
                            .unwrap()
                            .clone()
                            .lexeme
                            .unwrap();

                        let res = current_scope.bind(&lhs, rhs);

                        match res {
                            Ok(value) => value,
                            Err(e) => {
                                let msg = e.to_string();
                                self.error(msg);
                                GbType::Error
                            }
                        }
                    }
                    Token::OpGt => GbType::Boolean(left > right),
                    Token::OpLt => GbType::Boolean(left < right),
                    _ => {
                        self.error("Encountered binary operation, but operator token was not a binary operator");
                        return GbType::Error;
                    }
                }
            }
        }
    }

    fn evaluate_atom(&self, ast: AstNode, current_scope: &mut Scope) -> GbType {
        let Some(token) = ast.token else {
            self.error("Expected a Token,  but received None");
            return GbType::Error;
        };
        match token {
            Token::IntLiteral => {
                let Some(lexeme) = ast.lexeme else {
                    self.error(
                        "IntLiteral requires a lexeme but one was not provided",
                    );
                    return GbType::Error;
                };

                let val = str::parse::<i64>(&lexeme);

                let Ok(val) = val else {
                    self.error("IntLiteral could not be parsed as integer");
                    return GbType::Error;
                };

                return GbType::Integer(val);
            }
            Token::FloatLiteral => todo!(),
            Token::StringLiteral => todo!(),
            Token::Identifier => {
                let Some(lexeme) = ast.lexeme else {
                    self.error(
                        "Identifier requires a lexeme but one was not provided",
                    );
                    return GbType::Error;
                };

                let Ok(value) = current_scope.lookup(&lexeme) else {
                    self.error(format!(
                        "Identifier {} has not been defined",
                        lexeme
                    )); // TODO  handle language level errors separately from interpreter level errors
                    return GbType::Error;
                };

                Clone::clone(&value)
            }
            Token::Boolean => {
                let Some(lexeme) = ast.lexeme else {
                    self.error(
                        "Boolean requires a lexeme but one was not provided",
                    );
                    return GbType::Error;
                };

                let val = str::parse::<bool>(&lexeme);
                let Ok(val) = val else {
                    self.error("Boolean token could not be parsed as boolean");
                    return GbType::Error;
                };

                return GbType::Boolean(val);
            }
            Token::Keyword => todo!(),
            Token::OpAdd => todo!(),
            Token::OpSub => todo!(),
            Token::OpMul => todo!(),
            Token::OpDiv => todo!(),
            Token::OpExp => todo!(),
            Token::OpAssign => todo!(),
            Token::OpGt => todo!(),
            Token::OpLt => todo!(),
            Token::OpBang => todo!(),
            Token::LParen => todo!(),
            Token::RParen => todo!(),
            Token::LBrace => todo!(),
            Token::RBrace => todo!(),
            Token::LBracket => todo!(),
            Token::RBracket => todo!(),
            Token::Comma => todo!(),
            Token::Semicolon => todo!(),
            Token::Eol => todo!(),
            Token::Eof => todo!(),
        }
    }
}

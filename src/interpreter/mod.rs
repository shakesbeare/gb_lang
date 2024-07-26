pub mod environment;
pub mod gb_type;

use std::rc::Rc;

use self::environment::Environment;
use crate::{
    ast::{Expression, LetStatement, Node, Statement},
    parser::error::ParserError,
};
use gb_type::GbType;

pub trait InterpreterStrategy {
    fn evaluate(&mut self, input: &Node) -> &GbType;
}

pub struct Interpreter<T: InterpreterStrategy> {
    strategy: T,
    ast: Node,
}

impl<T: InterpreterStrategy> Interpreter<T> {
    pub fn new(strategy: T, input: String) -> Result<Self, ParserError> {
        let mut p = crate::parser::Parser::new(
            crate::lexer::Lexer::from(input.as_bytes()),
            Box::new(crate::parser::error::DefaultErrorHandler {
                input: input.to_string(),
            }),
            false,
        );
        let ast = p.parse()?;
        Ok(Self { strategy, ast })
    }

    fn evaluate(&mut self) {
        self.strategy.evaluate(&self.ast);
    }
}

pub struct TreeWalking {
    stack: Vec<Environment>,
}

impl InterpreterStrategy for TreeWalking {
    fn evaluate(&mut self, input: &Node) -> &GbType {
        match input {
            Node::Program(p) => self.evaluate_program(p.statements.as_slice()),
            Node::Statement(_) => todo!(),
            Node::Expression(_) => todo!(),
        }
    }
}

impl Default for TreeWalking {
    fn default() -> Self {
        let stack = vec![Environment::new()];
        Self::new(stack)
    }
}

impl TreeWalking {
    fn new(stack: Vec<Environment>) -> Self {
        Self { stack }
    }

    fn global_env(&mut self) -> &mut Environment {
        self.stack.get_mut(0).unwrap()
    }

    fn top_env(&mut self) -> &mut Environment {
        self.stack.last_mut().unwrap()
    }

    fn inspect(&self) -> Vec<Vec<(Rc<str>, &GbType)>> {
        let mut out = vec![];
        for env in self.stack.iter() {
            let kv = env.inspect();
            out.push(kv);
        }
        out
    }

    fn evaluate_program(&mut self, input: &[Node]) -> &GbType {
        for node in input {
            match node {
                Node::Program(_) => unreachable!(),
                Node::Statement(statement) => self.evaluate_statement(statement),
                Node::Expression(_) => unreachable!(),
            };
        }

        self.global_env().none()
    }

    fn evaluate_statement(&mut self, input: &Statement) -> &GbType {
        match input {
            Statement::LetStatement(ls) => self.evaluate_let_statement(ls),
            Statement::ReturnStatement(_) => todo!(),
            Statement::ExpressionStatement(_) => todo!(),
            Statement::BlockStatement(_) => todo!(),
            Statement::FunctionLiteralStatement(_) => todo!(),
        }
    }

    fn evaluate_let_statement(&mut self, input: &LetStatement) -> &GbType {
        let value = self.evaluate_expression(&input.value);
        self.top_env().insert(input.name.value(), value);
        self.global_env().none()
    }

    fn evaluate_expression(&mut self, input: &Expression) -> GbType {
        match input {
            Expression::Null => todo!(),
            Expression::Identifier(_) => todo!(),
            Expression::IntegerLiteral(i) => GbType::Integer(i.value),
            Expression::FloatLiteral(_) => todo!(),
            Expression::PrefixExpression(_) => todo!(),
            Expression::InfixExpression(_) => todo!(),
            Expression::BooleanLiteral(_) => todo!(),
            Expression::IfExpression(_) => todo!(),
            Expression::FunctionLiteral(_) => todo!(),
            Expression::CallExpression(_) => todo!(),
        }
    }
}

mod tests {
    #![allow(unused_imports)]
    use crate::{
        ast::{Expression, IntegerLiteral},
        interpreter::gb_type::GbType,
        lexer::Lexer,
        parser::{error::DefaultErrorHandler, Parser},
        token::{Point, Token, TokenKind},
    };

    use super::{Interpreter, InterpreterStrategy, TreeWalking};

    #[test]
    fn test_integer() {
        let input = Expression::IntegerLiteral(IntegerLiteral {
            token: Token {
                kind: TokenKind::IntLiteral,
                literal: "7".to_string(),
                location: Point { line: 1, col: 1 },
            },
            value: 7,
        });
        let res = TreeWalking::default().evaluate_expression(&input);
        assert_eq!(res, GbType::Integer(7));
    }

    #[test]
    fn eval_let_statement() {
        let input = "let x = 7;";
        let mut interpreter =
            Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
        interpreter.evaluate();
        let dump = interpreter.strategy.inspect();
        assert_eq!(dump.len(), 1);
        assert_eq!(dump[0].len(), 2);
        for (k, v) in dump[0].clone() {
            if k == "None".into() {
                assert_eq!(v, &GbType::None);
            } else if k == "x".into() {
                assert_eq!(v, &GbType::Integer(7));
            } else {
                panic!("Unexpected key: {}", k);
            }
        }
    }
}

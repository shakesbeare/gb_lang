pub mod environment;
pub mod gb_type;
mod tests;

use std::rc::Rc;

use self::environment::Environment;
use crate::{
    ast::{
        Alternative, BlockStatement, CallExpression, Expression, ExpressionStatement,
        FunctionLiteral, FunctionLiteralStatement, Identifier, IfExpression,
        InfixExpression, LetStatement, Node, PrefixExpression, Statement,
    },
    parser::error::ParserError,
};
use gb_type::{gb_pow, GbFunc, GbType};
use gxhash::{HashMap, HashMapExt};

pub trait InterpreterStrategy {
    fn evaluate(&mut self, input: &Node) -> GbType;
    fn new_env(&mut self);
    fn global_env(&mut self) -> &mut Environment;
    fn top_env(&mut self) -> &mut Environment;
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

    pub fn evaluate(&mut self) -> GbType {
        self.strategy.evaluate(&self.ast)
    }

    pub fn new_input(&mut self, input: String) -> Result<(), ParserError> {
        let mut p = crate::parser::Parser::new(
            crate::lexer::Lexer::from(input.as_bytes()),
            Box::new(crate::parser::error::DefaultErrorHandler {
                input: input.to_string(),
            }),
            false,
        );
        let ast = p.parse()?;
        self.ast = ast;
        Ok(())
    }
}

pub struct TreeWalking {
    stack: Vec<Environment>,
}

impl InterpreterStrategy for TreeWalking {
    fn evaluate(&mut self, input: &Node) -> GbType {
        match input {
            Node::Program(p) => self.evaluate_program(p.statements.as_slice()),
            Node::Statement(s) => self.evaluate_statement(s),
            Node::Expression(e) => self.evaluate_expression(e),
        }
    }

    fn new_env(&mut self) {
        self.stack.push(Environment::new(HashMap::new()))
    }

    fn global_env(&mut self) -> &mut Environment {
        self.stack.get_mut(0).unwrap()
    }

    fn top_env(&mut self) -> &mut Environment {
        self.stack.last_mut().unwrap()
    }
}

impl Default for TreeWalking {
    fn default() -> Self {
        let stack = vec![Environment::default()];
        Self::new(stack)
    }
}

impl TreeWalking {
    fn new(stack: Vec<Environment>) -> Self {
        Self { stack }
    }

    fn lookup(&mut self, key: Rc<str>) -> &GbType {
        let mut idx = self.stack.len() - 1;
        loop {
            let env = self.stack.get(idx).unwrap();
            let value = env.get(key.clone());
            if let Some(value) = value {
                return value;
            } else if idx > 0 {
                idx -= 1;
            } else {
                return &GbType::None;
            }
        }
    }

    fn inspect(&self) -> Vec<Vec<(Rc<str>, &GbType)>> {
        let mut out = vec![];
        for env in self.stack.iter() {
            let kv = env.inspect();
            out.push(kv);
        }
        out
    }

    fn evaluate_program(&mut self, input: &[Node]) -> GbType {
        let mut last_result = GbType::None;
        for node in input {
            match node {
                Node::Program(_) => unreachable!(),
                Node::Statement(statement) => {
                    last_result = self.evaluate_statement(statement)
                }
                Node::Expression(_) => unreachable!(),
            };
        }

        last_result
    }

    fn evaluate_statement(&mut self, input: &Statement) -> GbType {
        match input {
            Statement::LetStatement(ls) => self.evaluate_let_statement(ls),
            Statement::ReturnStatement(_) => todo!(),
            Statement::ExpressionStatement(es) => {
                self.evaluate_expression_statement(es)
            }
            Statement::BlockStatement(bs) => self.evaluate_block_statement(bs),
            Statement::FunctionLiteralStatement(fls) => {
                self.evaluate_function_literal_statement(fls)
            }
        }
    }

    fn evaluate_block_statement(&mut self, input: &BlockStatement) -> GbType {
        let mut last = GbType::None;
        for stmt in input.statements.iter() {
            last = self.evaluate_statement(stmt);
        }
        last
    }

    fn evaluate_expression_statement(&mut self, input: &ExpressionStatement) -> GbType {
        // this function exists in case an expression statement should
        // evaluate to something other than the result of its expression
        self.evaluate_expression(&input.expression)
    }

    fn evaluate_let_statement(&mut self, input: &LetStatement) -> GbType {
        let value = self.evaluate_expression(&input.value);
        self.top_env().insert(input.name.value(), value);
        GbType::Name(input.name.value().to_string())
    }

    fn evaluate_expression(&mut self, input: &Expression) -> GbType {
        match input {
            Expression::Identifier(id) => self.evaluate_identifier(id),
            Expression::IntegerLiteral(i) => GbType::Integer(i.value),
            Expression::FloatLiteral(f) => GbType::Float(f.value),
            Expression::StringLiteral(st) => GbType::String(st.value.clone()),
            Expression::PrefixExpression(pe) => self.evaluate_prefix_expression(pe),
            Expression::InfixExpression(ie) => self.evaluate_infix_expression(ie),
            Expression::BooleanLiteral(b) => GbType::Boolean(b.value),
            Expression::IfExpression(ie) => self.evaluate_if_expression(ie),
            Expression::FunctionLiteral(fl) => self.evaluate_function_literal(fl),
            Expression::CallExpression(fc) => self.evaluate_function_call(fc),
        }
    }

    fn evaluate_identifier(&mut self, input: &Identifier) -> GbType {
        self.lookup(input.value().into()).clone()
    }

    fn evaluate_prefix_expression(&mut self, expr: &PrefixExpression) -> GbType {
        match expr.operator.as_str() {
            "-" => GbType::Integer(-1) * self.evaluate_expression(&expr.right),
            "!" => !self.evaluate_expression(&expr.right),
            _ => unreachable!(),
        }
    }

    fn evaluate_infix_expression(&mut self, expr: &InfixExpression) -> GbType {
        match expr.operator.as_str() {
            "+" => {
                self.evaluate_expression(&expr.left)
                    + self.evaluate_expression(&expr.right)
            }
            "*" => {
                self.evaluate_expression(&expr.left)
                    * self.evaluate_expression(&expr.right)
            }
            "-" => {
                self.evaluate_expression(&expr.left)
                    - self.evaluate_expression(&expr.right)
            }
            "/" => {
                self.evaluate_expression(&expr.left)
                    / self.evaluate_expression(&expr.right)
            }
            ">" => GbType::Boolean(
                self.evaluate_expression(&expr.left)
                    > self.evaluate_expression(&expr.right),
            ),
            "<" => GbType::Boolean(
                self.evaluate_expression(&expr.left)
                    < self.evaluate_expression(&expr.right),
            ),
            "==" => GbType::Boolean(
                self.evaluate_expression(&expr.left)
                    == self.evaluate_expression(&expr.right),
            ),
            "!=" => GbType::Boolean(
                self.evaluate_expression(&expr.left)
                    != self.evaluate_expression(&expr.right),
            ),
            "**" => gb_pow(
                self.evaluate_expression(&expr.left),
                self.evaluate_expression(&expr.right),
            ),
            _ => unreachable!(),
        }
    }

    fn evaluate_if_expression(&mut self, input: &IfExpression) -> GbType {
        // pub token: Token,
        // pub condition: Rc<Expression>,
        // pub consequence: BlockStatement,
        // pub alternative: Option<BlockStatement>,

        let GbType::Boolean(cond) = self.evaluate_expression(&input.condition) else {
            return GbType::Error;
        };

        if cond {
            self.evaluate_block_statement(&input.consequence)
        } else {
            match &input.alternative {
                Alternative::IfExpression(ie) => self.evaluate_expression(ie),
                Alternative::BlockStatement(bs) => self.evaluate_block_statement(bs),
                Alternative::None => GbType::None,
            }
        }
    }

    fn evaluate_function_literal_statement(
        &mut self,
        input: &FunctionLiteralStatement,
    ) -> GbType {
        let key: Rc<str> = input.identifier.value().into();
        let value = GbType::Function(Rc::new(input.literal.clone()));
        self.top_env().insert(key, value);
        GbType::None
    }

    fn evaluate_function_literal(&mut self, input: &FunctionLiteral) -> GbType {
        GbType::Function(Rc::new(input.clone()))
    }

    fn evaluate_function_call(&mut self, input: &CallExpression) -> GbType {
        let Expression::Identifier(ref key) = *input.function else {
            // TODO error handling
            panic!("Expected Identifier, got {:?}", input.function);
        };

        let mut args = vec![];
        for arg in input.arguments.iter() {
            args.push(self.evaluate_expression(arg));
        }

        let GbType::Function(gb_func) = self.lookup(key.value().into()) else {
            // TODO error handling
            panic!(
                "Expected GbType::Function, got {:?}",
                self.lookup(key.value().into())
            );
        };
        // SAFETY:
        // functions will not be able to access their own entry in the symbol table
        let gb_func = unsafe { &*(&**gb_func as *const dyn GbFunc) };
        gb_func.execute(self, &args)
    }
}

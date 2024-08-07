pub mod environment;
pub mod gb_type;
mod tests;
mod lib;

use std::rc::Rc;

use self::environment::Environment;
use crate::{
    ast::{
        Alternative, BlockStatement, CallExpression, Expression, ExpressionStatement,
        FunctionLiteral, FunctionLiteralStatement, Identifier, IfExpression,
        InfixExpression, LetStatement, Node, PrefixExpression, Statement, WhileExpression,
    },
    parser::error::ParserError,
};
use gb_type::{gb_pow, GbFunc, GbType};
use gxhash::{HashMap, HashMapExt};

pub trait InterpreterStrategy {
    fn evaluate(&mut self, input: &Node, function_context: bool) -> GbType;
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
        self.strategy.evaluate(&self.ast, false)
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
    fn evaluate(&mut self, input: &Node, function_context: bool) -> GbType {
        match input {
            Node::Program(p) => {
                self.evaluate_program(p.statements.as_slice(), function_context)
            }
            Node::Statement(s) => self.evaluate_statement(s, function_context),
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

    fn lookup<T: Into<Rc<str>>>(&mut self, key: T) -> Option<&GbType> {
        let key = key.into();
        let mut idx = self.stack.len() - 1;
        loop {
            let env = self.stack.get(idx).unwrap();
            let value = env.get(key.clone());
            if let Some(value) = value {
                return Some(value);
            } else if idx > 0 {
                idx -= 1;
            } else {
                return None;
            }
        }
    }

    fn lookup_name_location<T: Into<Rc<str>>>(&mut self, key: T) -> Option<usize> {
        let key = key.into();
        let mut idx = self.stack.len() - 1;
        loop {
            let env = self.stack.get(idx).unwrap();
            let value = env.get(key.clone());
            if value.is_some() {
                return Some(idx);
            } else if idx > 0 {
                idx -= 1;
            } else {
                return None;
            }
        }

    }

    #[cfg(test)]
    fn inspect(&self) -> Vec<Vec<(Rc<str>, &GbType)>> {
        let mut out = vec![];
        for env in self.stack.iter() {
            let kv = env.inspect();
            out.push(kv);
        }
        out
    }

    fn evaluate_program(&mut self, input: &[Node], function_context: bool) -> GbType {
        let mut last_result = GbType::None;
        for node in input {
            match node {
                Node::Program(_) => unreachable!(),
                Node::Statement(statement) => {
                    last_result = self.evaluate_statement(statement, function_context)
                }
                Node::Expression(_) => unreachable!(),
            };
        }

        // SAFETY
        //     Function objects cannot be mutated
        let s = unsafe { &mut *(self as *mut Self)};
        if let Some(GbType::Function(main)) = self.global_env().get("main") {
            // TODO: 
            //     auto parse cli args as main args
            last_result = main.execute(s, &[]);
        }

        last_result
    }

    fn evaluate_statement(
        &mut self,
        input: &Statement,
        function_context: bool,
    ) -> GbType {
        match input {
            Statement::LetStatement(ls) => self.evaluate_let_statement(ls),
            Statement::ReturnStatement(rs) => {
                if !function_context {
                    panic!("Return is only allowed in function context");
                }
                self.evaluate_expression(&rs.return_value)
            }
            Statement::ExpressionStatement(es) => {
                self.evaluate_expression_statement(es)
            }
            Statement::BlockStatement(bs) => {
                self.evaluate_block_statement(bs, function_context)
            }
            Statement::FunctionLiteralStatement(fls) => {
                self.evaluate_function_literal_statement(fls)
            }
        }
    }

    fn evaluate_block_statement(
        &mut self,
        input: &BlockStatement,
        function_context: bool,
    ) -> GbType {
        let mut last = GbType::None;
        for stmt in input.statements.iter() {
            last = self.evaluate_statement(stmt, function_context);
            if stmt.is_return_statement() && function_context {
                return last;
            }
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
            Expression::WhileExpression(we) => self.evaluate_while_expression(we),
        }
    }

    fn evaluate_identifier(&mut self, input: &Identifier) -> GbType {
        if let Some(val) = self.lookup(input.value()) {
            val.clone()
        } else {
            panic!("Variable used before it was declared");

        }
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
            "=" => {
                let Expression::Identifier(ref key) = *expr.left else {
                    panic!("Cannot assign to {:?}", expr.left);
                };

                let Some(env_location) = self.lookup_name_location(key.value()) else {
                    panic!("Variable assigned to before it was declared");
                };
                let current_value = self.lookup(key.value()).unwrap();
                if let GbType::Function(_) = current_value {
                    panic!("Function types cannot be mutated");
                }

                let value = self.evaluate_expression(&expr.right);
                self.stack[env_location].insert(key.value(), value);

                GbType::None
            }
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
            self.evaluate_block_statement(&input.consequence, false)
        } else {
            match &input.alternative {
                Alternative::IfExpression(ie) => self.evaluate_expression(ie),
                Alternative::BlockStatement(bs) => {
                    self.evaluate_block_statement(bs, false)
                }
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

        let Some(GbType::Function(gb_func)) = self.lookup(key.value()) else {
            // TODO error handling
            panic!(
                "Expected GbType::Function, got {:?}",
                self.lookup(key.value())
            );
        };
        // SAFETY:
        // functions will not be able to access their own entry in the symbol table
        let gb_func = unsafe { &*(&**gb_func as *const dyn GbFunc) };
        gb_func.execute(self, &args)
    }

    fn evaluate_while_expression(&mut self, input: &WhileExpression) -> GbType {
        while self.evaluate_expression(&input.condition) == GbType::Boolean(true) {
            self.evaluate_block_statement(&input.body, false);
        }

        GbType::None
    }
}

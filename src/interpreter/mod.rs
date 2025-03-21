pub mod environment;
pub mod gb_type;
mod lib;
mod tests;

use std::rc::Rc;
use tracing::instrument;

use self::environment::Environment;
use crate::{
    ast::{
        Alternative, BlockStatement, CallExpression, Expression, ExpressionStatement,
        FunctionLiteral, FunctionLiteralStatement, Identifier, IfExpression, InfixExpression,
        LetStatement, Node, PrefixExpression, Statement, WhileExpression,
    },
    error::ErrorHandler,
    parser::error::ParserError,
    token::Token,
};
use gb_type::*;
use std::collections::HashMap;

pub trait InterpreterStrategy {
    fn eval(
        &mut self,
        input: &Node,
        function_context: bool,
        auto_main: bool,
    ) -> Result<GbType, GbError>;
    fn push_env(&mut self);
    fn builtins_env(&mut self) -> &mut Environment;
    fn top_env(&mut self) -> &mut Environment;
    fn stack(&self) -> &[Environment];
    //TODO: Use a result instead
    fn pop_env(&mut self) -> Option<()>;
    fn loose_envs(&self) -> &HashMap<Rc<str>, Environment>;
}

pub struct Interpreter<T: InterpreterStrategy> {
    strategy: T,
    ast: Node,
    error_handler: Box<dyn ErrorHandler>,
}

impl<T: InterpreterStrategy> Interpreter<T> {
    pub fn new(strategy: T, input: String) -> Result<Self, ParserError> {
        let mut p = crate::parser::Parser::new(
            crate::lexer::Lexer::from(input.as_bytes()),
            Box::new(crate::error::DefaultErrorHandler {
                input: input.to_string(),
            }),
            false,
        );
        let ast = p.parse()?;
        Ok(Self {
            strategy,
            ast,
            error_handler: Box::new(crate::error::DefaultErrorHandler { input }),
        })
    }

    pub fn new_lazy(strategy: T) -> Self {
        Self {
            strategy,
            ast: Node::Empty,
            error_handler: Box::new(crate::error::DefaultErrorHandler {
                input: String::from(""),
            }),
        }
    }

    pub fn evaluate(&mut self) -> Result<GbType, GbError> {
        let res = self.strategy.eval(&self.ast, false, true);
        if let Err(e) = res {
            println!(
                "{}",
                self.error_handler
                    .runtime_error(e.clone(), e.token.clone().unwrap())
            );
            return Err(e.clone());
        };
        res
    }

    pub fn eval_new_input<S>(&mut self, input: S) -> Result<(), ParserError>
    where
        S: AsRef<str>,
    {
        let mut p = crate::parser::Parser::new(
            crate::lexer::Lexer::from(input.as_ref().as_bytes()),
            Box::new(crate::error::DefaultErrorHandler {
                input: input.as_ref().to_string(),
            }),
            false,
        );
        self.error_handler.new_input(input.as_ref().to_string());
        let ast = p.parse()?;
        self.ast = ast;
        Ok(())
    }

    pub fn new_input<S>(&mut self, input: S)
    where
        S: AsRef<str>,
    {
        self.error_handler.new_input(input.as_ref().to_string());
    }

    pub fn eval_ast(&mut self, ast: &Node, auto_main: bool) -> Result<GbType, GbError> {
        let res = self.strategy.eval(ast, false, auto_main);
        if let Err(e) = res {
            println!(
                "{}",
                self.error_handler
                    .runtime_error(e.clone(), e.token.clone().unwrap())
            );
            return Err(e.clone());
        }
        res
    }
}

#[derive(Debug)]
pub struct TreeWalking {
    stack: Vec<Environment>,
    loose_envs: HashMap<Rc<str>, Environment>,
    loaded_files: Vec<String>,
}

/// Useful to automatically return if the evaluated function returns a GbType::ReturnValue
macro_rules! return_if_return (
    ($s:ident.$f:ident($($args:tt)*)) => {
        match $s.$f($($args)*) {
            Ok(GbType::ReturnValue(v)) => return Ok(GbType::ReturnValue(v)),
            Err(e) => return Err(e),
            x => x
        }
    }
);

impl InterpreterStrategy for TreeWalking {
    #[instrument(skip_all)]
    fn eval(
        &mut self,
        input: &Node,
        function_context: bool,
        auto_main: bool,
    ) -> Result<GbType, GbError> {
        Ok(match input {
            Node::Program(p) => {
                self.eval_prog(p.statements.as_slice(), function_context, auto_main)?
            }
            Node::Statement(s) => self.eval_stmt(s, function_context)?,
            Node::Expression(e) => self.eval_expr(e, function_context)?,
            Node::Empty => GbType::Empty,
        }
        .unwrap_return())
    }

    fn push_env(&mut self) {
        self.stack.push(Environment::new(HashMap::new()))
    }

    fn builtins_env(&mut self) -> &mut Environment {
        self.stack.get_mut(0).unwrap()
    }

    fn top_env(&mut self) -> &mut Environment {
        self.stack.last_mut().unwrap()
    }

    fn stack(&self) -> &[Environment] {
        &self.stack
    }

    fn loose_envs(&self) -> &HashMap<Rc<str>, Environment> {
        &self.loose_envs
    }

    fn pop_env(&mut self) -> Option<()> {
        self.stack.pop()?;
        Some(())
    }
}

impl Default for TreeWalking {
    fn default() -> Self {
        let stack = vec![Environment::default()];
        let mut tw = Self::new(stack);
        tw.push_env(); // ensure the builtins are always in their own environment
        tw
    }
}

impl TreeWalking {
    fn new(stack: Vec<Environment>) -> Self {
        Self {
            stack,
            loaded_files: vec![],
            loose_envs: HashMap::new(),
        }
    }

    fn lookup<T: Into<Rc<str>>>(&self, key: T) -> Option<&GbType> {
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

    fn dot_lookup(
        &mut self,
        parent: impl AsRef<str>,
        child: impl AsRef<str>,
        token: &Token,
    ) -> Result<GbType, GbError> {
        match self.lookup(parent.as_ref()) {
            Some(item) => self.namespace_lookup(token, item, parent, child),
            None => {
                let folder_name = parent.as_ref();
                let file_name = format!("{}.gb", parent.as_ref());
                if std::fs::exists(&file_name).unwrap() && !self.loaded_files.contains(&file_name) {
                    tracing::trace!("Found file");
                    let contents =
                        std::fs::read_to_string(&file_name).expect("Failed to read file");
                    let mut i = Interpreter::new(TreeWalking::default(), contents)
                        .expect("Failed to create sub interpreter");
                    i.evaluate()?;

                    self.loaded_files.push(file_name.clone());
                    let mut ns = HashMap::new();
                    for (k, v) in i.strategy.top_env().inspect() {
                        ns.insert(k.to_string(), v.clone().into());
                    }
                    tracing::trace!("Created namespace {}", parent.as_ref());
                    self.top_env()
                        .insert(parent.as_ref().to_string(), GbType::Namespace(ns));

                    self.dot_lookup(parent, child, token)
                } else if std::fs::exists(folder_name).unwrap() {
                    tracing::error!("Folders are not yet supported");
                    todo!()
                } else {
                    tracing::error!(
                        "Could not resolve lookup for {:?} in {:?}",
                        child.as_ref(),
                        parent.as_ref()
                    );
                    return Err(GbError {
                        token: Some(token.clone()),
                        kind: GbErrorKind::FailedToResolveNameLookup,
                    });
                }
            }
        }
    }

    fn namespace_lookup(
        &self,
        token: &Token,
        item: &GbType,
        parent: impl AsRef<str>,
        child: impl AsRef<str>,
    ) -> Result<GbType, GbError> {
        tracing::trace!(
            "Looking for attr {:?} in {:?}",
            child.as_ref(),
            parent.as_ref()
        );
        match item.get_attr(child.as_ref()) {
            Some(v) => Ok((*v).clone()),
            None => {
                tracing::error!("{} has no attribute {}", parent.as_ref(), child.as_ref());
                Err(GbError {
                    token: Some(token.clone()),
                    kind: GbErrorKind::FailedToResolveNameLookup,
                })
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

    #[instrument(skip_all)]
    fn eval_prog(
        &mut self,
        input: &[Node],
        function_context: bool,
        auto_main: bool,
    ) -> Result<GbType, GbError> {
        let mut last_result = GbType::None;
        for node in input {
            match node {
                Node::Program(_) => unreachable!(),
                Node::Statement(statement) => {
                    last_result = self.eval_stmt(statement, function_context)?
                }
                Node::Expression(_) => unreachable!(),
                Node::Empty => {
                    last_result = GbType::Empty;
                }
            };
        }

        if !auto_main {
            return Ok(last_result);
        }

        // SAFETY
        //     Function objects cannot be mutated
        let s = unsafe { &mut *(self as *mut Self) };
        if let Some(GbType::Function(main, _)) = self.top_env().get("main") {
            tracing::trace!("Found main function");
            // TODO:
            //     auto parse cli args as main args
            last_result = main.execute(s, &[], Token::eof(), None)?;
        }

        Ok(last_result)
    }

    #[instrument(skip_all)]
    fn eval_stmt(&mut self, input: &Statement, function_context: bool) -> Result<GbType, GbError> {
        match input {
            Statement::LetStatement(ls) => self.eval_let_stmt(ls, function_context),
            Statement::ReturnStatement(rs) => {
                if !function_context {
                    return Err(GbError {
                        token: Some(rs.token.clone()),
                        kind: GbErrorKind::MisplacedReturn,
                    });
                }
                self.eval_expr(&rs.return_value, function_context)
            }
            Statement::ExpressionStatement(es) => self.eval_expr_stmt(es, function_context),
            Statement::BlockStatement(bs) => {
                return_if_return!(self.eval_block_stmt(bs, function_context))
            }
            Statement::FunctionLiteralStatement(fls) => self.eval_fn_lit_stmt(fls),
        }
    }

    #[instrument(skip_all)]
    fn eval_block_stmt(
        &mut self,
        input: &BlockStatement,
        function_context: bool,
    ) -> Result<GbType, GbError> {
        let mut last = GbType::None;
        for stmt in input.statements.iter() {
            last = self.eval_stmt(stmt, function_context)?;
            if function_context {
                if stmt.is_return_stmt() {
                    tracing::trace!("Returning value");
                    return Ok(GbType::ReturnValue(last.into()));
                } else if gb_type::gb_type_of(&last) == "Return Value" {
                    tracing::trace!("Hoisting returned value");
                    return Ok(last.unwrap_return());
                }
            }
        }
        Ok(last)
    }

    #[instrument(skip_all)]
    fn eval_expr_stmt(
        &mut self,
        input: &ExpressionStatement,
        function_context: bool,
    ) -> Result<GbType, GbError> {
        // this function exists in case an expression statement should
        // evaluate to something other than the result of its expression
        self.eval_expr(&input.expression, function_context)
    }

    #[instrument(skip_all)]
    fn eval_let_stmt(
        &mut self,
        input: &LetStatement,
        function_context: bool,
    ) -> Result<GbType, GbError> {
        let value = self.eval_expr(&input.value, function_context)?;
        self.top_env().insert(input.name.value(), value);
        Ok(GbType::Name(input.name.value().to_string()))
    }

    #[instrument(skip_all)]
    fn eval_expr(&mut self, input: &Expression, function_context: bool) -> Result<GbType, GbError> {
        match input {
            Expression::Identifier(id) => self.eval_ident(id),
            Expression::IntegerLiteral(i) => Ok(GbType::Integer(i.value)),
            Expression::FloatLiteral(f) => Ok(GbType::Float(f.value)),
            Expression::StringLiteral(st) => Ok(GbType::String(st.value.clone())),
            Expression::PrefixExpression(pe) => self.eval_prefix_expr(pe, function_context),
            Expression::InfixExpression(ie) => self.eval_infix_expr(ie, function_context),
            Expression::BooleanLiteral(b) => Ok(GbType::Boolean(b.value)),
            Expression::IfExpression(ie) => self.eval_if_expr(ie, function_context),
            Expression::FunctionLiteral(fl) => self.eval_fn_lit(fl),
            Expression::CallExpression(fc) => self.eval_fn_call(fc, function_context),
            Expression::WhileExpression(we) => self.eval_while_expr(we, function_context),
        }
    }

    #[instrument(skip_all)]
    fn eval_ident(&mut self, input: &Identifier) -> Result<GbType, GbError> {
        if let Some(val) = self.lookup(input.value()) {
            tracing::trace!("Variable {:?} has value {:?}", input.token.literal, &val);
            Ok(val.clone())
        } else {
            return Err(GbError {
                token: Some(input.token.clone()),
                kind: GbErrorKind::VariableUsedBeforeDeclaration,
            });
        }
    }

    #[instrument(skip_all)]
    fn eval_prefix_expr(
        &mut self,
        expr: &PrefixExpression,
        function_context: bool,
    ) -> Result<GbType, GbError> {
        match expr.operator.as_str() {
            "-" => gb_mul(
                GbType::Integer(-1),
                self.eval_expr(&expr.right, function_context)?,
            ),
            "!" => gb_not(self.eval_expr(&expr.right, function_context)?),
            _ => unreachable!(),
        }
    }

    #[instrument(skip_all)]
    fn eval_infix_expr(
        &mut self,
        expr: &InfixExpression,
        function_context: bool,
    ) -> Result<GbType, GbError> {
        tracing::trace!("Doing {:?}", expr.operator.as_str());
        match expr.operator.as_str() {
            "+" => {
                let res = gb_add(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                if let Err(mut e) = res {
                    let _ = e.token.insert(expr.token.clone());
                    return Err(e);
                }
                res
            }
            "*" => {
                let res = gb_mul(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                if let Err(mut e) = res {
                    let _ = e.token.insert(expr.token.clone());
                    return Err(e);
                }
                res
            }
            "-" => {
                let res = gb_sub(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                if let Err(mut e) = res {
                    let _ = e.token.insert(expr.token.clone());
                    return Err(e);
                }
                res
            }
            "/" => {
                let res = gb_div(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                if let Err(mut e) = res {
                    let _ = e.token.insert(expr.token.clone());
                    return Err(e);
                }
                res
            }
            ">" => {
                let res = gb_cmp(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                match res {
                    Ok(ordering) => Ok(GbType::Boolean(matches!(
                        ordering,
                        Some(std::cmp::Ordering::Greater)
                    ))),
                    Err(mut e) => {
                        let _ = e.token.insert(expr.token.clone());
                        Err(e)
                    }
                }
            }
            "<" => {
                let res = gb_cmp(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                match res {
                    Ok(ordering) => Ok(GbType::Boolean(matches!(
                        ordering,
                        Some(std::cmp::Ordering::Less)
                    ))),
                    Err(mut e) => {
                        let _ = e.token.insert(expr.token.clone());
                        Err(e)
                    }
                }
            }
            "==" => {
                let res = gb_eq(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                match res {
                    Ok(equal) => Ok(GbType::Boolean(equal)),
                    Err(mut e) => {
                        let _ = e.token.insert(expr.token.clone());
                        Err(e)
                    }
                }
            }
            "!=" => {
                let res = gb_eq(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                match res {
                    Ok(equal) => Ok(GbType::Boolean(!equal)),
                    Err(mut e) => {
                        let _ = e.token.insert(expr.token.clone());
                        Err(e)
                    }
                }
            }
            "**" => {
                let res = gb_pow(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                if let Err(mut e) = res {
                    let _ = e.token.insert(expr.token.clone());
                    return Err(e);
                }
                res
            }
            "=" => {
                let Expression::Identifier(ref key) = *expr.left else {
                    return Err(GbError {
                        token: Some(expr.left.token()),
                        kind: GbErrorKind::VariableCannotBeAssignedToType,
                    });
                };

                let Some(env_location) = self.lookup_name_location(key.value()) else {
                    return Err(GbError {
                        token: Some(key.token.clone()),
                        kind: GbErrorKind::VariableUsedBeforeDeclaration,
                    });
                };
                let current_value = self.lookup(key.value()).unwrap();
                if let GbType::Function(_, _) = current_value {
                    return Err(GbError {
                        token: Some(key.token.clone()),
                        kind: GbErrorKind::FunctionMayNotBeMutated,
                    });
                }

                let curr = current_value.clone();
                let value = self.eval_expr(&expr.right, function_context)?;
                if !variant_eq(&curr, &value) {
                    return Err(GbError {
                        token: Some(expr.right.token()),
                        kind: GbErrorKind::VariableCannotBeAssignedToType,
                    });
                }
                self.stack[env_location].insert(key.value(), value);

                Ok(GbType::None)
            }
            ">=" => {
                let res = gb_cmp(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                match res {
                    Ok(ordering) => Ok(GbType::Boolean(matches!(
                        ordering,
                        Some(std::cmp::Ordering::Greater) | Some(std::cmp::Ordering::Equal)
                    ))),
                    Err(mut e) => {
                        let _ = e.token.insert(expr.token.clone());
                        Err(e)
                    }
                }
            }
            "<=" => {
                let res = gb_cmp(
                    self.eval_expr(&expr.left, function_context)?,
                    self.eval_expr(&expr.right, function_context)?,
                );
                match res {
                    Ok(ordering) => Ok(GbType::Boolean(matches!(
                        ordering,
                        Some(std::cmp::Ordering::Less) | Some(std::cmp::Ordering::Equal)
                    ))),
                    Err(mut e) => {
                        let _ = e.token.insert(expr.token.clone());
                        Err(e)
                    }
                }
            }
            "." => {
                let Expression::Identifier(ref parent) = *expr.left else {
                    return Err(GbError {
                        token: Some(expr.token.clone()),
                        kind: GbErrorKind::DotLookupOnlyApplicableToIdentifiers,
                    });
                };
                let Expression::Identifier(ref child) = *expr.right else {
                    return Err(GbError {
                        token: Some(expr.token.clone()),
                        kind: GbErrorKind::DotLookupOnlyApplicableToIdentifiers,
                    });
                };
                self.dot_lookup(parent.to_string(), child.to_string(), &expr.token)
            }
            _ => unreachable!(),
        }
    }

    #[instrument(skip_all)]
    fn eval_if_expr(
        &mut self,
        input: &IfExpression,
        function_context: bool,
    ) -> Result<GbType, GbError> {
        // pub token: Token,
        // pub condition: Rc<Expression>,
        // pub consequence: BlockStatement,
        // pub alternative: Option<BlockStatement>,

        let GbType::Boolean(cond) = self.eval_expr(&input.condition, function_context)? else {
            return Err(GbError {
                token: Some(input.token.clone()),
                kind: GbErrorKind::ConditionalMustEvaluateToBool,
            });
        };

        if cond {
            tracing::trace!("Condition was true");
            return_if_return!(self.eval_block_stmt(&input.consequence, function_context))
        } else {
            tracing::trace!("Condition was false");
            match &input.alternative {
                Alternative::IfExpression(ie) => self.eval_expr(ie, function_context),
                Alternative::BlockStatement(bs) => {
                    return_if_return!(self.eval_block_stmt(bs, function_context))
                }
                Alternative::None => Ok(GbType::None),
            }
        }
    }

    #[instrument(skip_all)]
    fn eval_fn_lit_stmt(&mut self, input: &FunctionLiteralStatement) -> Result<GbType, GbError> {
        let key: Rc<str> = input.identifier.value().into();
        let top_env = self.top_env().clone();
        let value = GbType::Function(Rc::new(input.literal.clone()), Some(top_env));
        self.top_env().insert(key.clone(), value);
        tracing::trace!("Created Function: {:?}", key);
        Ok(GbType::None)
    }

    #[instrument(skip_all)]
    fn eval_fn_lit(&mut self, input: &FunctionLiteral) -> Result<GbType, GbError> {
        let top_env = self.top_env().clone();
        Ok(GbType::Function(Rc::new(input.clone()), Some(top_env)))
    }

    #[instrument(skip_all)]
    fn eval_fn_call(
        &mut self,
        input: &CallExpression,
        function_context: bool,
    ) -> Result<GbType, GbError> {
        let (func, gb_func, env) = match *input.function {
            Expression::Identifier(ref key) => {
                let Some(GbType::Function(gb_func, env)) = self.lookup(key.value()) else {
                    return Err(GbError {
                        token: Some(input.token.clone()),
                        kind: GbErrorKind::AttemptedToCallNonFunctionType,
                    });
                };
                // SAFETY:
                // functions will not be able to access their own entry in the symbol table
                (key.to_string(), unsafe {
                    &*(&**gb_func as *const dyn GbFunc)
                }, env.clone())
            }
            Expression::InfixExpression(ref ie) => {
                let val = self.eval_infix_expr(ie, function_context)?;
                let GbType::Function(gb_func, env) = val else {
                    return Err(GbError {
                        token: Some(input.token.clone()),
                        kind: GbErrorKind::AttemptedToCallNonFunctionType,
                    });
                };
                (ie.to_string(), unsafe {
                    &*(&*gb_func as *const dyn GbFunc)
                }, env)
            }
            _ => {
                return Err(GbError {
                    token: Some(input.token.clone()),
                    kind: GbErrorKind::AttemptedToCallNonFunctionType,
                });
            }
        };

        let mut args = vec![];
        for arg in input.arguments.iter() {
            args.push(self.eval_expr(arg, function_context)?);
        }

        tracing::trace!("Calling function {:?} with args {:?}", func, &args);
        Ok(gb_func
            .execute(self, &args, input.token.clone(), env)?
            .unwrap_return())
    }

    #[instrument(skip_all)]
    fn eval_while_expr(
        &mut self,
        input: &WhileExpression,
        function_context: bool,
    ) -> Result<GbType, GbError> {
        while matches!(
            gb_eq(
                self.eval_expr(&input.condition, function_context)?,
                GbType::Boolean(true)
            ),
            Ok(true)
        ) {
            let _ = return_if_return!(self.eval_block_stmt(&input.body, false));
        }

        Ok(GbType::None)
    }
}

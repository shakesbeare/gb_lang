pub mod error;
mod tests;

use crate::{
    ast::*,
    lexer::LexStatus,
    token::{HasKind, TokenKind},
};
use std::{collections::HashMap, io::Read};
use std::{ops::Deref, rc::Rc};

use crate::{lexer::Lexer, token::Token};

use self::error::{ErrorHandler, ParserError};

type PrefixParseFn<'a, R> = fn(&mut Parser<'a, R>) -> Result<Expression, ParserError>;
type InfixParseFn<'a, R> =
    fn(&mut Parser<'a, R>, Expression) -> Result<Expression, ParserError>;

#[allow(dead_code)]
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Copy)]
enum Precedence {
    Lowest,
    Assign,
    Equals,
    LessGreater,
    Sum,
    Product,
    Exponent,
    Prefix,
    Call,
}

pub struct Parser<'a, R: Read> {
    /// If true, the parser will print debug information
    pub verbose: bool,
    pub lexer: Lexer<R>,
    cur_token: Rc<Token>,
    peek_token: Rc<Token>,
    error_handler: Box<dyn ErrorHandler>,
    pub errors: Vec<String>,

    prefix_parse_fns: HashMap<TokenKind, PrefixParseFn<'a, R>>,
    infix_parse_fns: HashMap<TokenKind, InfixParseFn<'a, R>>,

    precedences: HashMap<TokenKind, Precedence>,
}

impl<'a, R: Read> Parser<'a, R> {
    pub fn new(
        mut lexer: Lexer<R>,
        error_handler: Box<dyn ErrorHandler>,
        verbose: bool,
    ) -> Self {
        let LexStatus::Reading { token } = lexer.lex() else {
            panic!("Failed to read first token");
        };
        let first = token;
        let second: Token;
        if let LexStatus::Reading { token } = lexer.lex() {
            second = token
        } else {
            second = Token::eof();
        }
        let mut p = Self {
            lexer,
            verbose,
            cur_token: Rc::new(first),
            peek_token: Rc::new(second),
            errors: Vec::new(),
            error_handler,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences: HashMap::new(),
        };

        p.precedences.insert(TokenKind::Assign, Precedence::Assign);
        p.precedences.insert(TokenKind::Equals, Precedence::Equals);
        p.precedences
            .insert(TokenKind::NotEquals, Precedence::Equals);
        p.precedences
            .insert(TokenKind::LessThan, Precedence::LessGreater);
        p.precedences
            .insert(TokenKind::GreaterThan, Precedence::LessGreater);
        p.precedences.insert(TokenKind::Add, Precedence::Sum);
        p.precedences.insert(TokenKind::Subtract, Precedence::Sum);
        p.precedences.insert(TokenKind::Divide, Precedence::Product);
        p.precedences
            .insert(TokenKind::Multiply, Precedence::Product);
        p.precedences
            .insert(TokenKind::Exponentiate, Precedence::Exponent);
        p.precedences.insert(TokenKind::LParen, Precedence::Call);

        p.register_prefix(TokenKind::Identifier, Parser::parse_identifier);
        p.register_prefix(TokenKind::IntLiteral, Parser::parse_integer_literal);
        p.register_prefix(TokenKind::FloatLiteral, Parser::parse_float_literal);
        p.register_prefix(TokenKind::StringLiteral, Parser::parse_string_literal);
        p.register_prefix(TokenKind::Bang, Parser::parse_prefix_expression);
        p.register_prefix(TokenKind::Subtract, Parser::parse_prefix_expression);
        p.register_prefix(TokenKind::True, Parser::parse_boolean);
        p.register_prefix(TokenKind::False, Parser::parse_boolean);
        p.register_prefix(TokenKind::LParen, Parser::parse_grouped_expression);
        p.register_prefix(TokenKind::If, Parser::parse_if_expression);
        p.register_prefix(TokenKind::Fn, Parser::parse_function_literal);
        p.register_prefix(TokenKind::While, Parser::parse_while_expression);

        p.register_infix(TokenKind::Add, Parser::parse_infix_expression);
        p.register_infix(TokenKind::Subtract, Parser::parse_infix_expression);
        p.register_infix(TokenKind::Divide, Parser::parse_infix_expression);
        p.register_infix(TokenKind::Multiply, Parser::parse_infix_expression);
        p.register_infix(TokenKind::Equals, Parser::parse_infix_expression);
        p.register_infix(TokenKind::NotEquals, Parser::parse_infix_expression);
        p.register_infix(TokenKind::LessThan, Parser::parse_infix_expression);
        p.register_infix(TokenKind::GreaterThan, Parser::parse_infix_expression);
        p.register_infix(TokenKind::LParen, Parser::parse_call_expression);
        p.register_infix(TokenKind::Exponentiate, Parser::parse_infix_expression);
        p.register_infix(TokenKind::Assign, Parser::parse_infix_expression);

        return p;
    }

    // Useful for debugging, set verbose to true
    fn print<T: std::fmt::Debug>(&self, item: T) {
        if self.verbose {
            println!("{:?}", item);
        }
    }

    fn next_token(&mut self) {
        if let LexStatus::Reading { token } = self.lexer.lex() {
            self.cur_token = self.peek_token.clone();
            self.peek_token = token.into();
        } else if let LexStatus::Eof = self.lexer.lex() {
            self.cur_token = self.peek_token.clone();
            if !self.peek_token.has_kind(TokenKind::Eof) {
                self.peek_token = Rc::new(Token::eof());
            }
        }
    }

    fn peek_precedence(&mut self) -> Precedence {
        if let Some(p) = self.precedences.get(&self.peek_token.kind) {
            return *p;
        } else {
            return Precedence::Lowest;
        }
    }

    fn cur_precedence(&mut self) -> Precedence {
        if let Some(p) = self.precedences.get(&self.cur_token.kind) {
            return *p;
        } else {
            return Precedence::Lowest;
        }
    }

    /// Advances to the next token if the peek token has the expected kind
    fn expect_peek(&mut self, kind: TokenKind) -> Result<(), ParserError> {
        if self.peek_token.has_kind(kind) {
            self.next_token();
            Ok(())
        } else {
            Err(self.syntax_error(self.peek_token.as_ref().clone()))
        }
    }

    fn syntax_error(&mut self, token: Token) -> ParserError {
        let e_string = self.error_handler.syntax_error(token);
        self.errors.push(e_string.clone());
        ParserError::SyntaxError(e_string)
    }

    pub fn check_parser_errors(&self) {
        if self.errors.is_empty() {
            return;
        }

        panic!("Parser encountered errors\n");
    }

    fn register_prefix(&mut self, kind: TokenKind, func: PrefixParseFn<'a, R>) {
        self.prefix_parse_fns.insert(kind, func);
    }

    fn register_infix(&mut self, kind: TokenKind, func: InfixParseFn<'a, R>) {
        self.infix_parse_fns.insert(kind, func);
    }

    fn recover(&mut self) {
        // try to find the end of the current erroneous section
        // this is probably a ; or a closing delimiter
        //    but need to pay attention to inner sets of delimiters
        // Example:
        // https://github.com/rust-lang/rust/blob/master/compiler/rustc_parse/src/parser/diagnostics.rs#L2078

        let mut brace_depth = 0;
        let mut bracket_depth = 0;
        let mut in_block = false;
        loop {
            match self.cur_token.kind {
                TokenKind::Semicolon => {
                    self.next_token();
                    if brace_depth == 0 && bracket_depth == 0 {
                        break;
                    }
                }
                TokenKind::LBrace => {
                    brace_depth += 1;
                    self.next_token();
                    if brace_depth == 1 && bracket_depth == 0 {
                        in_block = true;
                    }
                }
                TokenKind::LBracket => {
                    bracket_depth += 1;
                    self.next_token();
                }
                TokenKind::RBrace => {
                    brace_depth -= 1;
                    if brace_depth < 0 {
                        brace_depth = 0;
                    }
                    self.next_token();
                    if in_block && bracket_depth == 0 && brace_depth == 0 {
                        break;
                    }
                }
                TokenKind::RBracket => {
                    bracket_depth -= 1;
                    if bracket_depth < 0 {
                        bracket_depth = 0;
                    }
                    self.next_token();
                }
                TokenKind::Eof => {
                    break;
                }
                _ => {
                    self.next_token();
                }
            }
        }
    }

    /// Parses the entire program and returns the abstract syntax tree
    /// This also saturates the fields of the internal lexer.
    pub fn parse(&mut self) -> Result<Node, ParserError> {
        self.print("start program");
        let mut program = Program {
            statements: Vec::new(),
        };

        while !self.cur_token.has_kind(TokenKind::Eof) {
            if let Some(stmt) = self.parse_statement() {
                if let Ok(stmt) = stmt {
                    program.statements.push(Node::Statement(stmt));
                } else {
                    let err = stmt.unwrap_err();
                    eprintln!("{}", err);
                    self.recover();
                }
            }

            self.next_token();
        }

        self.print("end program");
        return Ok(Node::Program(program));
    }

    fn parse_statement(&mut self) -> Option<Result<Statement, ParserError>> {
        match self.cur_token.kind {
            TokenKind::Let => Some(self.parse_let_statement()),
            TokenKind::Fn => Some(self.parse_function_literal_statement()),
            TokenKind::Return => Some(self.parse_return_statement()),
            TokenKind::Eol => None,
            _ => Some(self.parse_expression_statement()),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.cur_token.as_ref().clone();
        self.expect_peek(TokenKind::Identifier)?;
        let name = Identifier {
            token: self.cur_token.as_ref().clone(),
        };
        self.expect_peek(TokenKind::Assign)?;
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token.has_kind(TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::LetStatement(LetStatement {
            token,
            name,
            value: value.into(),
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.cur_token.as_ref().clone();
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token.has_kind(TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::ReturnStatement(ReturnStatement {
            token,
            return_value: value.into(),
        }))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.cur_token.as_ref().clone();
        let expression = self.parse_expression(Precedence::Lowest);
        if self.peek_token.has_kind(TokenKind::Semicolon) {
            self.next_token();
        }
        Ok(Statement::ExpressionStatement(ExpressionStatement {
            token,
            expression: Rc::new(expression?),
        }))
    }

    fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<Expression, ParserError> {

        let Some(prefix) = self.prefix_parse_fns.get(&self.cur_token.kind) else {
            return Err(self.syntax_error(self.cur_token.deref().clone()));
        };

        let mut left_exp = prefix(self);

        while !self.peek_token.has_kind(TokenKind::Semicolon)
            && precedence < self.peek_precedence()
        {
            let p_self = unsafe {
                // SAFETY:
                //     - `infix` is not used until after `p_self` is done being used
                // Reasoning:
                //     - Using unsafe here avoids matching the pattern below twice
                &mut *(self as *mut Parser<R>)
            };
            let Some(infix) = self.infix_parse_fns.get(&self.peek_token.kind) else {
                return left_exp;
            };
            p_self.next_token();

            left_exp = infix(self, left_exp?);
        }

        left_exp
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::Identifier(Identifier {
            token: self.cur_token.as_ref().clone(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::IntegerLiteral(IntegerLiteral {
            token: self.cur_token.as_ref().clone(),
            value: self.cur_token.literal.parse().unwrap(),
        }))
    }

    fn parse_float_literal(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::FloatLiteral(FloatLiteral {
            token: self.cur_token.as_ref().clone(),
            value: self.cur_token.literal.parse().unwrap(),
        }))
    }

    fn parse_string_literal(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::StringLiteral(StringLiteral {
            token: self.cur_token.as_ref().clone(),
            value: self.cur_token.literal.parse().unwrap(),
        }))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self.cur_token.as_ref().clone();
        let op = token.literal.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::PrefixExpression(PrefixExpression {
            token,
            operator: op,
            right: Rc::new(right),
        }))
    }

    fn parse_infix_expression(
        &mut self,
        left: Expression,
    ) -> Result<Expression, ParserError> {
        let token = self.cur_token.as_ref().clone();
        let op = token.literal.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(Expression::InfixExpression(InfixExpression {
            token,
            operator: op,
            left: Rc::new(left),
            right: Rc::new(right),
        }))
    }

    fn parse_boolean(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::BooleanLiteral(BooleanLiteral {
            token: self.cur_token.as_ref().clone(),
            value: self.cur_token.has_kind(TokenKind::True),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest);
        self.expect_peek(TokenKind::RParen)?;

        exp
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self.cur_token.as_ref().clone();
        self.next_token();
        let cond = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::LBrace)?;
        let consequence = self.parse_block_statement()?;
        let mut alternative = Alternative::None;

        if self.peek_token.has_kind(TokenKind::Else) {
            self.next_token();

            alternative = if self.peek_token.has_kind(TokenKind::LBrace) {
                self.next_token();
                Alternative::BlockStatement(self.parse_block_statement()?)
            } else if self.peek_token.has_kind(TokenKind::If) {
                self.next_token();
                Alternative::IfExpression(self.parse_if_expression()?.into())
            } else {
                return Err(self.syntax_error(self.peek_token.as_ref().clone()));
            };
        }

        Ok(Expression::IfExpression(IfExpression {
            token,
            condition: Rc::new(cond),
            consequence,
            alternative,
        }))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParserError> {
        let token = self.cur_token.as_ref().clone();
        let mut statements = Vec::new();
        self.next_token();

        while !self.cur_token.has_kind(TokenKind::RBrace)
            && !self.cur_token.has_kind(TokenKind::Eof)
        {
            if let Some(stmt) = self.parse_statement() {
                statements.push(Rc::new(stmt?));
            }
            self.next_token();
        }

        Ok(BlockStatement { token, statements })
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        let token = self.cur_token.as_ref().clone();

        self.expect_peek(TokenKind::LParen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(TokenKind::LBrace)?;
        let body = self.parse_block_statement()?;

        Ok(Expression::FunctionLiteral(FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }

    fn parse_function_literal_statement(&mut self) -> Result<Statement, ParserError> {
        // check if this is a fn() {}, otherwise fn main() {}
        if !self.peek_token.has_kind(TokenKind::Identifier) {
            return self.parse_expression_statement();
        }

        let token = self.cur_token.as_ref().clone();
        self.next_token();

        let Expression::Identifier(identifier) = self.parse_identifier()? else {
            unreachable!()
        };

        self.expect_peek(TokenKind::LParen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(TokenKind::LBrace)?;
        let body = self.parse_block_statement()?;

        let function_literal = FunctionLiteral {
            token: token.clone(),
            parameters,
            body
        };

        return Ok(FunctionLiteralStatement {
            token,
            identifier,
            literal: function_literal,
        }
        .into_statement());
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParserError> {
        let mut identifiers = Vec::new();
        if self.peek_token.has_kind(TokenKind::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        let ident = Identifier {
            token: self.cur_token.as_ref().clone(),
        };

        identifiers.push(ident);

        while self.peek_token.has_kind(TokenKind::Comma) {
            self.next_token(); // cur_token is now comma
            self.next_token(); // cur_token is now identifier
            let ident = Identifier {
                token: self.cur_token.as_ref().clone(),
            };
            identifiers.push(ident);
        }

        self.expect_peek(TokenKind::RParen)?;
        Ok(identifiers)
    }

    fn parse_call_expression(
        &mut self,
        function: Expression,
    ) -> Result<Expression, ParserError> {
        let token = self.cur_token.as_ref().clone();
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::CallExpression(CallExpression {
            token,
            function: Rc::new(function),
            arguments,
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut args = vec![];

        if self.peek_token.has_kind(TokenKind::RParen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token.has_kind(TokenKind::Comma) {
            self.next_token(); // cur_token is now comma
            self.next_token(); // cur_token is now expression
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(TokenKind::RParen)?;
        Ok(args)
    }

    fn parse_while_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self.cur_token.as_ref().clone();
        self.next_token();
        let condition = Rc::new(self.parse_expression(Precedence::Lowest)?);
        self.expect_peek(TokenKind::LBrace)?;
        let body = Rc::new(self.parse_block_statement()?);

        Ok(Expression::WhileExpression(WhileExpression {
            token,
            condition,
            body,
        }))
    }
}

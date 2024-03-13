use crate::{
    ast::*,
    lexer::LexStatus,
    token::{HasKind, TokenKind},
};
use std::rc::Rc;
use std::{collections::HashMap, io::Read};

use crate::{lexer::Lexer, token::Token};

type PrefixParseFn<R> = fn(&mut Parser<R>) -> Expression;
type InfixParseFn<R> = fn(&mut Parser<R>, Expression) -> Expression;

#[allow(dead_code)]
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Copy)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Exponent,
    Prefix,
    Call,
}

pub struct Parser<R: Read> {
    /// If true, the parser will print debug information
    pub verbose: bool,
    pub lexer: Lexer<R>,
    cur_token: Rc<Token>,
    peek_token: Rc<Token>,
    errors: Vec<String>,

    prefix_parse_fns: HashMap<TokenKind, PrefixParseFn<R>>,
    infix_parse_fns: HashMap<TokenKind, InfixParseFn<R>>,

    precedences: HashMap<TokenKind, Precedence>,
}

impl<R: Read> Parser<R> {
    pub fn new(mut lexer: Lexer<R>, verbose: bool) -> Self {
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
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences: HashMap::new(),
        };

        p.precedences.insert(TokenKind::Equals, Precedence::Equals);
        p.precedences.insert(TokenKind::NotEquals, Precedence::Equals);
        p.precedences
            .insert(TokenKind::LessThan, Precedence::LessGreater);
        p.precedences
            .insert(TokenKind::GreaterThan, Precedence::LessGreater);
        p.precedences.insert(TokenKind::Add, Precedence::Sum);
        p.precedences.insert(TokenKind::Subtract, Precedence::Sum);
        p.precedences.insert(TokenKind::Divide, Precedence::Product);
        p.precedences.insert(TokenKind::Multiply, Precedence::Product);
        p.precedences.insert(TokenKind::Exponentiate, Precedence::Exponent);
        p.precedences.insert(TokenKind::LParen, Precedence::Call);

        p.register_prefix(TokenKind::Identifier, Parser::parse_identifier);
        p.register_prefix(TokenKind::IntLiteral, Parser::parse_integer_literal);
        p.register_prefix(TokenKind::FloatLiteral, Parser::parse_float_literal);
        p.register_prefix(TokenKind::Bang, Parser::parse_prefix_expression);
        p.register_prefix(TokenKind::Subtract, Parser::parse_prefix_expression);
        p.register_prefix(TokenKind::True, Parser::parse_boolean);
        p.register_prefix(TokenKind::False, Parser::parse_boolean);
        p.register_prefix(TokenKind::LParen, Parser::parse_grouped_expression);
        p.register_prefix(TokenKind::If, Parser::parse_if_expression);
        p.register_prefix(TokenKind::Fn, Parser::parse_function_literal);

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
    fn expect_peek(&mut self, kind: TokenKind) -> bool {
        if self.peek_token.has_kind(kind) {
            self.next_token();
            return true;
        } else {
            self.syntax_error(self.peek_token.as_ref().clone());
            return false;
        }
    }

    fn syntax_error(&mut self, token: Token) {
        let e_string = format!(
            "Encountered a syntax error at {}, {}: Unexpected {:?}",
            self.lexer.line, self.lexer.col, token
        );
        eprintln!("{}", e_string);
        self.errors.push(e_string);
    }

    fn no_prefix_parse_function_error(&mut self, kind: TokenKind) {
        let e_string = format!("No prefix parse function for {:?} found", kind);
        eprintln!("{}", e_string);
        self.errors.push(e_string);
    }

    pub fn check_parser_errors(&self) {
        if self.errors.is_empty() {
            return;
        }

        let errs = self.errors.join("\n");
        panic!("Parser encountered errors:\n{}", errs);
    }

    fn register_prefix(&mut self, kind: TokenKind, func: PrefixParseFn<R>) {
        self.prefix_parse_fns.insert(kind, func);
    }

    fn register_infix(&mut self, kind: TokenKind, func: InfixParseFn<R>) {
        self.infix_parse_fns.insert(kind, func);
    }

    /// Parses the entire program and returns the abstract syntax tree
    /// This also saturates the fields of the internal lexer.
    pub fn parse(&mut self) -> Node {
        self.print("start program");
        let mut program = Program {
            statements: Vec::new(),
        };

        while !self.cur_token.has_kind(TokenKind::Eof) {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(Node::Statement(stmt));
            }

            self.next_token();
        }

        self.print("end program");
        return Node::Program(program);
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Eol => None,
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.as_ref().clone();
        self.expect_peek(TokenKind::Identifier);
        let name = Identifier {
            token: self.cur_token.as_ref().clone(),
        };
        self.expect_peek(TokenKind::Assign);
        self.next_token();
        let value = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or(Expression::Null);

        if self.peek_token.has_kind(TokenKind::Semicolon) {
            self.next_token();
        }

        Some(Statement::LetStatement(LetStatement {
            token,
            name,
            value: value.into(),
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.as_ref().clone();
        self.next_token();
        let value = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or(Expression::Null);

        if self.peek_token.has_kind(TokenKind::Semicolon) {
            self.next_token();
        }

        Some(Statement::ReturnStatement(ReturnStatement {
            token,
            return_value: value.into(),
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.as_ref().clone();
        let expression = self.parse_expression(Precedence::Lowest);
        if self.peek_token.has_kind(TokenKind::Semicolon) {
            self.next_token();
        }
        Some(Statement::ExpressionStatement(ExpressionStatement {
            token,
            expression: Rc::new(expression?),
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let Some(prefix) = self.prefix_parse_fns.get(&self.cur_token.kind) else {
            self.no_prefix_parse_function_error(self.cur_token.kind);
            return None;
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
                return Some(left_exp);
            };
            p_self.next_token();

            left_exp = infix(self, left_exp);
        }

        return Some(left_exp);
    }

    fn parse_identifier(&mut self) -> Expression {
        Expression::Identifier(Identifier {
            token: self.cur_token.as_ref().clone(),
        })
    }

    fn parse_integer_literal(&mut self) -> Expression {
        Expression::IntegerLiteral(IntegerLiteral {
            token: self.cur_token.as_ref().clone(),
            value: self.cur_token.literal.parse().unwrap(),
        })
    }

    fn parse_float_literal(&mut self) -> Expression {
        Expression::FloatLiteral(FloatLiteral {
            token: self.cur_token.as_ref().clone(),
            value: self.cur_token.literal.parse().unwrap(),
        })
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let token = self.cur_token.as_ref().clone();
        let op = token.literal.clone();
        self.next_token();
        let Some(right) = self.parse_expression(Precedence::Prefix) else {
            return Expression::Null;
        };
        Expression::PrefixExpression(PrefixExpression {
            token,
            operator: op,
            right: Rc::new(right),
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let token = self.cur_token.as_ref().clone();
        let op = token.literal.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let Some(right) = self.parse_expression(precedence) else {
            return Expression::Null;
        };
        Expression::InfixExpression(InfixExpression {
            token,
            operator: op,
            left: Rc::new(left),
            right: Rc::new(right),
        })
    }

    fn parse_boolean(&mut self) -> Expression {
        Expression::Boolean(BooleanLiteral {
            token: self.cur_token.as_ref().clone(),
            value: self.cur_token.has_kind(TokenKind::True),
        })
    }

    fn parse_grouped_expression(&mut self) -> Expression {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(TokenKind::RParen) {
            return Expression::Null;
        }

        return exp.unwrap_or(Expression::Null);
    }

    fn parse_if_expression(&mut self) -> Expression {
        let token = self.cur_token.as_ref().clone();
        self.next_token();
        let Some(cond) = self.parse_expression(Precedence::Lowest) else {
            return Expression::Null;
        };

        if !self.expect_peek(TokenKind::LBrace) {
            return Expression::Null;
        }

        let consequence = self.parse_block_statement();
        let mut alternative: Option<BlockStatement> = None;

        if self.peek_token.has_kind(TokenKind::Else) {
            self.next_token();

            if !self.expect_peek(TokenKind::LBrace) {
                return Expression::Null;
            }

            alternative = Some(self.parse_block_statement());
        }

        return Expression::IfExpression(IfExpression {
            token,
            condition: Rc::new(cond),
            consequence,
            alternative,
        });
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = self.cur_token.as_ref().clone();
        let mut statements = Vec::new();
        self.next_token();

        while !self.cur_token.has_kind(TokenKind::RBrace)
            && !self.cur_token.has_kind(TokenKind::Eof)
        {
            if let Some(stmt) = self.parse_statement() {
                statements.push(Rc::new(stmt));
            }
            self.next_token();
        }

        return BlockStatement { token, statements };
    }

    fn parse_function_literal(&mut self) -> Expression {
        let token = self.cur_token.as_ref().clone();

        if !self.expect_peek(TokenKind::LParen) {
            return Expression::Null;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenKind::LBrace) {
            return Expression::Null;
        }

        let body = self.parse_block_statement();

        return Expression::FunctionLiteral(FunctionLiteral {
            token,
            parameters,
            body,
        });
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::new();
        if self.peek_token.has_kind(TokenKind::RParen) {
            self.next_token();
            return identifiers;
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

        if !self.expect_peek(TokenKind::RParen) {
            return Vec::new();
        }

        return identifiers;
    }

    fn parse_call_expression(&mut self, function: Expression) -> Expression {
        let token = self.cur_token.as_ref().clone();
        let arguments = self.parse_call_arguments();
        return Expression::CallExpression(CallExpression {
            token,
            function: Rc::new(function),
            arguments,
        });
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut args = vec![];

        if self.peek_token.has_kind(TokenKind::RParen) {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(
            self.parse_expression(Precedence::Lowest)
                .unwrap_or(Expression::Null),
        );

        while self.peek_token.has_kind(TokenKind::Comma) {
            self.next_token(); // cur_token is now comma
            self.next_token(); // cur_token is now expression
            args.push(
                self.parse_expression(Precedence::Lowest)
                    .unwrap_or(Expression::Null),
            );
        }

        if !self.expect_peek(TokenKind::RParen) {
            self.syntax_error(self.peek_token.as_ref().clone());
            return vec![];
        }

        return args;
    }
}

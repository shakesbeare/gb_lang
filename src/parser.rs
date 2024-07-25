use crate::{
    ast::*,
    error::ErrorHandler,
    lexer::LexStatus,
    token::{HasKind, TokenKind},
};
use std::{ops::Deref, rc::Rc};
use std::{collections::HashMap, io::Read};

use crate::{lexer::Lexer, token::Token};

type PrefixParseFn<'a, R> = fn(&mut Parser<'a, R>) -> Expression;
type InfixParseFn<'a, R> = fn(&mut Parser<'a, R>, Expression) -> Expression;

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

pub struct Parser<'a, R: Read> {
    /// If true, the parser will print debug information
    pub verbose: bool,
    pub lexer: Lexer<R>,
    cur_token: Rc<Token>,
    peek_token: Rc<Token>,
    error_handler: ErrorHandler<'a>,
    pub errors: Vec<String>,

    prefix_parse_fns: HashMap<TokenKind, PrefixParseFn<'a, R>>,
    infix_parse_fns: HashMap<TokenKind, InfixParseFn<'a, R>>,

    precedences: HashMap<TokenKind, Precedence>,
}

impl<'a, R: Read> Parser<'a, R> {
    pub fn new(
        mut lexer: Lexer<R>,
        error_handler: ErrorHandler<'a>,
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
        let e_string = self.error_handler.syntax_error(token);
        eprintln!("{}", e_string);
        self.errors.push(e_string);
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
            TokenKind::Fn => self.parse_function_literal_statement(),
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
            self.syntax_error(self.cur_token.deref().clone());
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
        Expression::BooleanLiteral(BooleanLiteral {
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

    fn parse_function_literal_statement(&mut self) -> Option<Statement> {
        // check if this is a fn() {}, otherwise fn main() {}
        if !self.peek_token.has_kind(TokenKind::Identifier) {
            return self.parse_expression_statement();
        }

        let token = self.cur_token.as_ref().clone();
        self.next_token();

        let Expression::Identifier(identifier) = self.parse_identifier() else {
            unreachable!()
        };
        // self.next_token();

        if !self.expect_peek(TokenKind::LParen) {
            self.syntax_error(self.peek_token.as_ref().clone());
            return None;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenKind::LBrace) {
            self.syntax_error(self.peek_token.as_ref().clone());
            return None;
        }

        let body = self.parse_block_statement();

        return Some(
            FunctionLiteralStatement {
                token,
                identifier,
                parameters,
                body,
            }
            .into_statement(),
        );
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

mod tests {
    #![allow(unused_imports)]

    use super::Expression;
    use crate::{
        ast::{IntoExpression, Node, Statement},
        error::ErrorHandler,
        lexer::Lexer,
        parser::Parser,
        token::TokenKind,
    };

    #[cfg(test)]
    fn test_integer_literal(exp: Expression, expected: i64) {
        if let Expression::IntegerLiteral(ref lit) = exp {
            assert_eq!(lit.value, expected);
            assert_eq!(lit.token.literal, expected.to_string());
        } else {
            panic!("Expected IntegerLiteral, got {:?}", exp);
        }
    }

    #[cfg(test)]
    fn test_float_literal(exp: Expression, expected: f64) {
        if let Expression::FloatLiteral(ref lit) = exp {
            assert_eq!(lit.value, expected);
        } else {
            panic!("Expected IntegerLiteral, got {:?}", exp);
        }
    }

    #[cfg(test)]
    fn test_boolean_literal(exp: Expression, expected: bool) {
        if let Expression::BooleanLiteral(ref lit) = exp {
            assert_eq!(lit.value, expected);
            assert_eq!(lit.token.literal, expected.to_string());
        } else {
            panic!("Expected Boolean, got {:?}", exp);
        }
    }

    #[cfg(test)]
    fn test_identifier(exp: Expression, expected: &str) {
        if let Expression::Identifier(ref ident) = exp {
            assert_eq!(ident.value(), expected);
            assert_eq!(ident.token.literal, expected);
        } else {
            panic!("Expected Identifier, got {:?}", exp);
        }
    }

    #[cfg(test)]
    fn test_infix_expression<T>(
        exp: Expression,
        expected_left: T,
        operator: &str,
        expected_right: T,
    ) where
        T: std::fmt::Debug + PartialEq + std::fmt::Display,
    {
        if let Expression::InfixExpression(ref infix) = exp {
            assert_eq!(infix.operator, operator);
            let left = match *infix.left {
                Expression::IntegerLiteral(ref lit) => lit.value.to_string(),
                Expression::Identifier(ref ident) => ident.value().to_string(),
                Expression::BooleanLiteral(ref lit) => lit.value.to_string(),
                _ => panic!(
                    "Expected IntegerLiteral or Identifier, got {:?}",
                    infix.left
                ),
            };

            let right = match *infix.right {
                Expression::IntegerLiteral(ref lit) => lit.value.to_string(),
                Expression::Identifier(ref ident) => ident.value().to_string(),
                Expression::BooleanLiteral(ref lit) => lit.value.to_string(),
                _ => panic!(
                    "Expected IntegerLiteral or Identifier, got {:?}",
                    infix.right
                ),
            };
            assert_eq!(left, expected_left.to_string());
            assert_eq!(right, expected_right.to_string());
        } else {
            panic!("Expected InfixExpression, got {:?}", exp);
        }
    }

    #[test]
    fn let_statements() {
        let input: Vec<(&str, &str, i64)> = vec![
            ("let x = 5;", "x", 5),
            ("let y = 10;", "y", 10),
            ("let foobar = 838383;", "foobar", 838383),
        ];

        for (inp, expected_identifier, expected_value) in input {
            let mut parser = Parser::new(
                Lexer::from(inp.as_bytes()),
                ErrorHandler { input: inp },
                false,
            );
            let ast = parser.parse();
            parser.check_parser_errors();

            let children = ast.into_program().statements;
            assert_eq!(children.len(), 1);

            let Node::Statement(Statement::LetStatement(ref stmt)) = children[0] else {
                panic!("Expected LetStatement, got {:?}", children[0]);
            };

            assert_eq!(stmt.token.kind, TokenKind::Let);
            assert_eq!(stmt.token.literal, "let");
            test_identifier(
                Expression::Identifier(stmt.name.clone()),
                expected_identifier,
            );
            test_integer_literal((*stmt.value).clone(), expected_value);
        }
    }

    #[test]
    fn return_statements() {
        let input: Vec<(&str, i64)> = vec![
            ("return 5;", 5),
            ("return 10;", 10),
            ("return 838383;", 838383),
        ];

        for (inp, expected) in input {
            let mut parser = Parser::new(
                Lexer::from(inp.as_bytes()),
                ErrorHandler { input: inp },
                false,
            );
            let ast = parser.parse();
            parser.check_parser_errors();

            let children = ast.into_program().statements;
            assert_eq!(children.len(), 1);

            let Node::Statement(Statement::ReturnStatement(ref stmt)) = children[0]
            else {
                panic!("Expected ReturnStatement, got {:?}", children[0]);
            };

            assert_eq!(stmt.token.kind, TokenKind::Return);
            assert_eq!(stmt.token.literal, "return");
            test_integer_literal((*stmt.return_value).clone(), expected);
        }
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar;".as_bytes();
        let mut parser = Parser::new(
            Lexer::from(input),
            ErrorHandler {
                input: std::str::from_utf8(input).unwrap(),
            },
            false,
        );
        let ast = parser.parse();
        parser.check_parser_errors();

        let children = ast.into_program().statements;
        assert_eq!(children.len(), 1);

        let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
        else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };

        test_identifier((*stmt.expression).clone(), "foobar");
    }

    #[test]
    fn integer_literal_expression() {
        let input = "5;".as_bytes();
        let mut parser = Parser::new(
            Lexer::from(input),
            ErrorHandler {
                input: std::str::from_utf8(input).unwrap(),
            },
            false,
        );
        let ast = parser.parse();
        parser.check_parser_errors();

        let children = ast.into_program().statements;
        assert_eq!(children.len(), 1);

        let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
        else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };
        test_integer_literal((*stmt.expression).clone(), 5);
    }

    #[test]
    fn float_literal_expression() {
        let input: Vec<(&str, f64)> = vec![("5.0;", 5.0), ("5.;", 5.0), ("0.5;", 0.5)];
        for (inp, expected) in input {
            dbg!(&inp);
            let mut parser = Parser::new(
                Lexer::from(inp.as_bytes()),
                ErrorHandler { input: inp },
                false,
            );
            let ast = parser.parse();
            parser.check_parser_errors();

            let children = ast.into_program().statements;
            assert_eq!(children.len(), 1);

            let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
            else {
                panic!("Expected ExpressionStatement, got {:?}", children[0]);
            };
            test_float_literal((*stmt.expression).clone(), expected);
        }
    }

    #[test]
    fn prefix_expression_1() {
        let input: Vec<(&str, &str, i64)> = vec![("!5;", "!", 5), ("-15;", "-", 15)];

        for (inp, op, int) in input {
            let mut parser = Parser::new(
                Lexer::from(inp.as_bytes()),
                ErrorHandler { input: inp },
                false,
            );
            let ast = parser.parse();
            let children = ast.into_program().statements;
            parser.check_parser_errors();
            assert_eq!(children.len(), 1);
            let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
            else {
                panic!("Expected ExpressionStatement, got {:?}", children[0]);
            };
            let Expression::PrefixExpression(ref prefix) = *stmt.expression else {
                panic!("Expected PrefixExpression, got {:?}", stmt.expression);
            };
            assert_eq!(prefix.operator, op);
            test_integer_literal((*prefix.right).clone(), int);
        }
    }

    #[test]
    fn prefix_expression_2() {
        let input: Vec<(&str, &str, bool)> =
            vec![("!true;", "!", true), ("!false", "!", false)];

        for (inp, op, boolean) in input {
            let mut parser = Parser::new(
                Lexer::from(inp.as_bytes()),
                ErrorHandler { input: inp },
                false,
            );
            let ast = parser.parse();
            let children = ast.into_program().statements;
            parser.check_parser_errors();
            assert_eq!(children.len(), 1);
            let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
            else {
                panic!("Expected ExpressionStatement, got {:?}", children[0]);
            };
            let Expression::PrefixExpression(ref prefix) = *stmt.expression else {
                panic!("Expected PrefixExpression, got {:?}", stmt.expression);
            };
            assert_eq!(prefix.operator, op);
            test_boolean_literal((*prefix.right).clone(), boolean);
        }
    }

    #[test]
    fn infix_expression_1() {
        let input: Vec<(&str, i64, &str, i64)> = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
            ("5 ** 5;", 5, "**", 5),
        ];

        for (inp, left, op, right) in input {
            let mut parser = Parser::new(
                Lexer::from(inp.as_bytes()),
                ErrorHandler { input: inp },
                false,
            );
            let ast = parser.parse();
            let children = ast.into_program().statements;
            parser.check_parser_errors();
            assert_eq!(children.len(), 1);
            let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
            else {
                panic!("Expected ExpressionStatement, got {:?}", children[0]);
            };

            test_infix_expression((*stmt.expression).clone(), left, op, right);
        }
    }

    #[test]
    fn infix_expression_2() {
        let input: Vec<(&str, bool, &str, bool)> = vec![
            ("true == true", true, "==", true),
            ("true != false", true, "!=", false),
            ("false == false", false, "==", false),
        ];

        for (inp, left, op, right) in input {
            let mut parser = Parser::new(
                Lexer::from(inp.as_bytes()),
                ErrorHandler { input: inp },
                false,
            );
            let ast = parser.parse();
            let children = ast.into_program().statements;
            parser.check_parser_errors();
            assert_eq!(children.len(), 1);
            let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
            else {
                panic!("Expected ExpressionStatement, got {:?}", children[0]);
            };

            test_infix_expression((*stmt.expression).clone(), left, op, right);
        }
    }

    #[test]
    fn infix_expression_3() {
        let input: Vec<(&str, &str, &str, &str)> = vec![
            ("foobar + barfoo;", "foobar", "+", "barfoo"),
            ("foobar - barfoo;", "foobar", "-", "barfoo"),
            ("foobar * barfoo;", "foobar", "*", "barfoo"),
            ("foobar / barfoo;", "foobar", "/", "barfoo"),
            ("foobar ** barfoo;", "foobar", "**", "barfoo"),
            ("foobar > barfoo;", "foobar", ">", "barfoo"),
            ("foobar < barfoo;", "foobar", "<", "barfoo"),
            ("foobar == barfoo;", "foobar", "==", "barfoo"),
            ("foobar != barfoo;", "foobar", "!=", "barfoo"),
        ];

        for (inp, left, op, right) in input {
            let mut parser = Parser::new(
                Lexer::from(inp.as_bytes()),
                ErrorHandler { input: inp },
                false,
            );
            let ast = parser.parse();
            let children = ast.into_program().statements;
            parser.check_parser_errors();
            assert_eq!(children.len(), 1);
            let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
            else {
                panic!("Expected ExpressionStatement, got {:?}", children[0]);
            };

            test_infix_expression((*stmt.expression).clone(), left, op, right);
        }
    }

    #[test]
    fn operator_precedence() {
        let input: Vec<(&str, &str)> = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            ("5 * 4 ** 2", "(5 * (4 ** 2))"),
        ];

        for (inp, expected) in input {
            let mut parser = Parser::new(
                Lexer::from(inp.as_bytes()),
                ErrorHandler { input: inp },
                false,
            );
            let ast = parser.parse();
            parser.check_parser_errors();
            let actual = ast.to_string();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn boolean_expression() {
        let input = "true;".as_bytes();
        let mut parser = Parser::new(
            Lexer::from(input),
            ErrorHandler {
                input: std::str::from_utf8(input).unwrap(),
            },
            false,
        );
        let ast = parser.parse();
        parser.check_parser_errors();

        let children = ast.into_program().statements;
        assert_eq!(children.len(), 1);

        let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
        else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };
        test_boolean_literal((*stmt.expression).clone(), true);
    }

    #[test]
    fn if_expression() {
        let input = "if x < y { x }";
        let mut parser =
            Parser::new(Lexer::from(input.as_bytes()), ErrorHandler { input }, false);
        let ast = parser.parse();
        parser.check_parser_errors();

        let children = ast.into_program().statements;
        assert_eq!(children.len(), 1);

        let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
        else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };

        let Expression::IfExpression(ref if_expr) = *stmt.expression else {
            panic!("Expected IfExpression, got {:?}", stmt.expression);
        };

        test_infix_expression((*if_expr.condition).clone(), "x", "<", "y");

        let Statement::ExpressionStatement(ref expr_stmt) =
            *if_expr.consequence.statements[0]
        else {
            panic!(
                "Expected ExpressionStatement, got {:?}",
                if_expr.consequence.statements[0]
            );
        };

        test_identifier((*expr_stmt.expression).clone(), "x");

        if if_expr.alternative.is_some() {
            panic!("Expected no alternative, got {:?}", if_expr.alternative);
        }
    }

    #[test]
    fn if_else_expression() {
        let input = "if x < y { x } else { y }";
        let mut parser =
            Parser::new(Lexer::from(input.as_bytes()), ErrorHandler { input }, false);
        let ast = parser.parse();
        parser.check_parser_errors();

        let children = ast.into_program().statements;
        assert_eq!(children.len(), 1);

        let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
        else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };

        let Expression::IfExpression(ref if_expr) = *stmt.expression else {
            panic!("Expected IfExpression, got {:?}", stmt.expression);
        };

        test_infix_expression((*if_expr.condition).clone(), "x", "<", "y");

        let Statement::ExpressionStatement(ref expr_stmt) =
            *if_expr.consequence.statements[0]
        else {
            panic!(
                "Expected ExpressionStatement, got {:?}",
                if_expr.consequence.statements[0]
            );
        };

        test_identifier((*expr_stmt.expression).clone(), "x");

        let Statement::ExpressionStatement(ref expr_stmt) =
            *if_expr.alternative.as_ref().unwrap().statements[0]
        else {
            panic!(
                "Expected ExpressionStatement, got {:?}",
                if_expr.alternative.as_ref().unwrap().statements[0]
            );
        };

        test_identifier((*expr_stmt.expression).clone(), "y");
    }

    #[test]
    fn function_literal() {
        let input = "fn(x, y) { x + y; }";

        let mut parser =
            Parser::new(Lexer::from(input.as_bytes()), ErrorHandler { input }, false);
        let ast = parser.parse();
        parser.check_parser_errors();

        let children = ast.into_program().statements;
        assert_eq!(children.len(), 1);

        let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
        else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };

        let Expression::FunctionLiteral(ref func) = *stmt.expression else {
            panic!("Expected FunctionLiteral, got {:?}", stmt.expression);
        };

        if func.parameters.len() != 2 {
            panic!("Expected 2 parameters, got {:?}", func.parameters);
        }

        let expected_params = ["x", "y"];
        for (i, param) in func.parameters.iter().enumerate() {
            assert_eq!(param.value(), expected_params[i]);
        }

        let Statement::ExpressionStatement(ref expr_stmt) = *func.body.statements[0]
        else {
            panic!(
                "Expected ExpressionStatement, got {:?}",
                func.body.statements[0]
            );
        };

        test_infix_expression((*expr_stmt.expression).clone(), "x", "+", "y");
    }

    #[test]
    fn function_parameters() {
        let input: Vec<(&str, &[&str])> = vec![
            ("fn() {};", &[]),
            ("fn(x) {};", &["x"]),
            ("fn(x, y, z) {};", &["x", "y", "z"]),
        ];

        for (inp, expected) in input {
            let mut parser = Parser::new(
                Lexer::from(inp.as_bytes()),
                ErrorHandler { input: inp },
                false,
            );
            let ast = parser.parse();
            parser.check_parser_errors();

            let children = ast.into_program().statements;
            assert_eq!(children.len(), 1);

            let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
            else {
                panic!("Expected ExpressionStatement, got {:?}", children[0]);
            };

            let Expression::FunctionLiteral(ref func) = *stmt.expression else {
                panic!("Expected FunctionLiteral, got {:?}", stmt.expression);
            };

            if func.parameters.len() != expected.len() {
                panic!(
                    "Expected {} parameters, got {:?}",
                    expected.len(),
                    func.parameters
                );
            }

            for (i, param) in func.parameters.iter().enumerate() {
                assert_eq!(param.value(), expected[i]);
            }
        }
    }

    #[test]
    fn call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let mut parser =
            Parser::new(Lexer::from(input.as_bytes()), ErrorHandler { input }, false);
        let ast = parser.parse();
        parser.check_parser_errors();

        let children = ast.into_program().statements;
        assert_eq!(children.len(), 1);

        let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
        else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };

        let Expression::CallExpression(ref call) = *stmt.expression else {
            panic!("Expected CallExpression, got {:?}", stmt.expression);
        };

        test_identifier((*call.function).clone(), "add");

        if call.arguments.len() != 3 {
            panic!("Expected 3 arguments, got {:?}", call.arguments);
        }

        let expected_args = ["1", "(2 * 3)", "(4 + 5)"];
        for (i, arg) in call.arguments.iter().enumerate() {
            assert_eq!(arg.to_string(), expected_args[i]);
        }
    }

    #[test]
    fn function_literal_statement() {
        let input = "fn main(x, y) { x + y; }";
        let mut parser =
            Parser::new(Lexer::from(input.as_bytes()), ErrorHandler { input }, false);
        let ast = parser.parse();
        parser.check_parser_errors();

        let children = ast.into_program().statements;
        assert_eq!(children.len(), 1);

        let Node::Statement(Statement::FunctionLiteralStatement(ref func)) =
            children[0]
        else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };

        test_identifier(func.identifier.clone().into_expression(), "main");

        if func.parameters.len() != 2 {
            panic!("Expected 2 parameters, got {:?}", func.parameters);
        }

        let expected_params = ["x", "y"];
        for (i, param) in func.parameters.iter().enumerate() {
            assert_eq!(param.value(), expected_params[i]);
        }

        let Statement::ExpressionStatement(ref expr_stmt) = *func.body.statements[0]
        else {
            panic!(
                "Expected ExpressionStatement, got {:?}",
                func.body.statements[0]
            );
        };

        test_infix_expression((*expr_stmt.expression).clone(), "x", "+", "y");
    }
}

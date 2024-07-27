#![allow(unused_imports)]
use std::rc::Rc;

use crate::{
    ast::{
        BlockStatement, BooleanLiteral, Expression, FloatLiteral, IntegerLiteral,
        Statement,
    },
    interpreter::gb_type::GbType,
    lexer::Lexer,
    parser::{error::DefaultErrorHandler, Parser},
    token::{Point, Token, TokenKind},
};

use super::{gb_type::GbFunc, Interpreter, InterpreterStrategy, TreeWalking};

pub trait AsAny {
    fn as_any(&self) -> &dyn std::any::Any;
}

impl AsAny for Rc<dyn GbFunc> {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[test]
fn integer() {
    let input = "7;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(7));
}

#[test]
fn float() {
    let input = "7.0;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Float(7.0));
}

#[test]
fn boolean() {
    let input = "true;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Boolean(true));
    i.new_input("false;".to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Boolean(false));
}

#[test]
fn string() {
    let input = [
        ("'hello'", GbType::String("hello".to_string())),
        ("\"hello\"", GbType::String("hello".to_string())),
    ];

    for (input, expected) in input {
        let mut i =
            Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
        let actual = i.evaluate();
        assert_eq!(actual, expected);
    }
}

#[test]
fn let_statement() {
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

#[test]
fn identifer() {
    let input = "let x = 7; x;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(7));
}

#[test]
fn prefix_expression() {
    let inputs = [
        ("-7;", GbType::Integer(-7)),
        ("!true;", GbType::Boolean(false)),
    ];

    for (input, expected) in inputs {
        let mut i =
            Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
        let actual = i.evaluate();
        assert_eq!(actual, expected);
    }
}

#[test]
fn infix_expression() {
    let input = [
        ("5 + 5;", GbType::Integer(10)),
        ("5 - 5;", GbType::Integer(0)),
        ("5 * 5;", GbType::Integer(25)),
        ("5 / 5;", GbType::Integer(1)),
        ("5 > 5;", GbType::Boolean(false)),
        ("5 < 5;", GbType::Boolean(false)),
        ("5 == 5;", GbType::Boolean(true)),
        ("5 != 5;", GbType::Boolean(false)),
        ("5 ** 5;", GbType::Integer(3125)),
        ("true == true", GbType::Boolean(true)),
        ("true != false", GbType::Boolean(true)),
        ("true == false", GbType::Boolean(false)),
        ("false == false", GbType::Boolean(true)),
        ("let x = 7; let y = 7; x == y;", GbType::Boolean(true)),
    ];

    for (input, expected) in input {
        let mut i =
            Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
        let actual = i.evaluate();
        dbg!(input);
        dbg!(&actual, &expected);
        assert_eq!(actual, expected);
    }
}

#[test]
fn if_expression() {
    let input = [
        ("if 3 < 5 { 7 } else { 7 }", GbType::Integer(7)),
        ("if 5 < 3 { 7 } else { 8 }", GbType::Integer(8)),
    ];

    for (input, expected) in input {
        let mut i =
            Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
        let actual = i.evaluate();
        assert_eq!(actual, expected);
    }
}

// #[test]
// fn function_literal_statement() {
//     let input = "fn main() { 7 }";
//     let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
//     let res = i.evaluate();
//     let GbType::Function(ptr) = res else {
//         panic!("Expected GbType::Function, got {:?}", res);
//     };
//     let map = &i.strategy.stack[0];
//     assert!(map.get("main").is_some());
//     let bs  = match ptr.as_any().downcast_ref::<BlockStatement>() {
//         Some(bs) => bs,
//         None => panic!("Inner function type was not a block statement node"),
//     };
//
//     let actual = i.strategy.evaluate_block_statement(bs);
//     assert_eq!(actual, GbType::Integer(7));
// }

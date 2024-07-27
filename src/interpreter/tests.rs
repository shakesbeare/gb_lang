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

#[test]
fn function_literal_statement() {
    let input = "fn foo(x, y) { x + y }";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    let map = &i.strategy.stack[0];
    let GbType::Function(ptr) = map.get("foo").unwrap().clone() else {
        panic!("Expected GbType::Function, got {:?}", res);
    };
    let actual = ptr.execute(&mut i.strategy, &[GbType::Integer(3), GbType::Integer(4)]);
    assert_eq!(actual, GbType::Integer(7));
}

#[test]
fn function_literal() {
    let input = "fn (x, y) { x + y }";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    let GbType::Function(ptr) = res else {
        panic!("Expected GbType::Function, got {:?}", res);
    };
    let actual = ptr.execute(&mut i.strategy, &[GbType::Integer(3), GbType::Integer(4)]);
    assert_eq!(actual, GbType::Integer(7));
}

#[test]
fn function_call() {
    let input = "fn main(x, y) { x + y } main(3, 4)";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(7));
}

#[test]
fn return_statement() {
    let input = "fn main() { 7; return 8; 9; } main()";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(8));

    let input = "fn foo() { 7; return 8; 9; } fn bar() { 1; return foo(); 3; } fn main() { 4; return bar(); 5;} main();";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(8));
}

#[test]
#[should_panic]
fn invalid_return_statements() {
    let input = "return 7;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    i.evaluate();
}

#[test]
fn assignment() {
    let input = "let x = 7; x = 8; x";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(8));
}

#[test]
#[should_panic]
fn bad_assignment() {
    let input = "x = 7;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    i.evaluate();
}

#[test]
#[should_panic]
fn variable_use_before_declaration() {
    let input = "x";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    i.evaluate();
}

#[test]
fn while_expression() {
    let input = "let x = 0; while x < 10 { x = x + 1 }; x ";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(10));
}

#[test]
#[should_panic]
fn disallow_mutating_functions() {
    let input = "fn foo() {} foo = 7;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    i.evaluate();
}

#[test]
fn auto_exec_global_main() {
    let input = "fn main() { 7 }";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(7));
}

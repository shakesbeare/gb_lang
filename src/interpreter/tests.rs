#![allow(unused_imports)]
use std::rc::Rc;

use tracing_test::traced_test;

use crate::{
    ast::{BlockStatement, BooleanLiteral, Expression, FloatLiteral, IntegerLiteral, Statement},
    interpreter::gb_type::GbType,
    lexer::Lexer,
    parser::{error::DefaultErrorHandler, Parser},
    token::{Point, Token, TokenKind},
};

use super::{gb_type::GbFunc, Interpreter, InterpreterStrategy, TreeWalking};

#[test]
#[traced_test]
fn integer() {
    let input = "7;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(7));
}

#[test]
#[traced_test]
fn float() {
    let input = "7.0;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Float(7.0));
}

#[test]
#[traced_test]
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
#[traced_test]
fn string() {
    let input = [
        ("'hello'", GbType::String("hello".to_string())),
        ("\"hello\"", GbType::String("hello".to_string())),
    ];

    for (input, expected) in input {
        let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
        let actual = i.evaluate();
        assert_eq!(actual, expected);
    }
}

#[test]
#[traced_test]
fn let_statement() {
    let input = "let x = 7;";
    let mut interpreter = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    interpreter.evaluate();
    let dump = interpreter.strategy.inspect();
    println!("{:?}", dump);
    assert_eq!(dump.len(), 2);
    assert_eq!(dump[1].len(), 1);
    for (k, v) in dump[1].clone() {
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
#[traced_test]
fn identifer() {
    let input = "let x = 7; x;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(7));
}

#[test]
#[traced_test]
fn prefix_expression() {
    let inputs = [
        ("-7;", GbType::Integer(-7)),
        ("!true;", GbType::Boolean(false)),
    ];

    for (input, expected) in inputs {
        let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
        let actual = i.evaluate();
        assert_eq!(actual, expected);
    }
}

#[test]
#[traced_test]
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
        ("0 <= 1", GbType::Boolean(true)),
        ("1 <= 1", GbType::Boolean(true)),
        ("0 >= 1", GbType::Boolean(false)),
        ("1 >= 1", GbType::Boolean(true)),
        ("let x = 7; let y = 7; x == y;", GbType::Boolean(true)),
    ];

    for (input, expected) in input {
        let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
        let actual = i.evaluate();
        if actual != expected {
            tracing::info!("Input: {:?}", input);
            tracing::info!("Actual: {:?}, Expected: {:?}", &actual, &expected);
        }
        assert_eq!(actual, expected);
    }
}

#[test]
#[traced_test]
fn if_expression() {
    let input = [
        ("if 3 < 5 { 7 } else { 8 }", GbType::Integer(7)),
        ("if 3 < 5 { 7 } else if 3 > 5 { 8 }", GbType::Integer(7)),
        ("if 5 < 3 { 7 } else { 8 }", GbType::Integer(8)),
        ("if 5 < 3 { 7 } else if 5 > 3 { 8 }", GbType::Integer(8)),
        ("if 5 < 3 { 7 } else if 5 > 3 { 8 }", GbType::Integer(8)),
        (
            "if 5 < 3 { 7 } else if 5 < 3 { 8 } else { 9 }",
            GbType::Integer(9),
        ),
    ];

    for (input, expected) in input {
        let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
        let actual = i.evaluate();
        assert_eq!(actual, expected);
    }
}

#[test]
#[traced_test]
fn function_literal_statement() {
    let input = "fn foo(x, y) { x + y }";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    let map = &i.strategy.top_env();
    let GbType::Function(ptr) = map.get("foo").unwrap().clone() else {
        panic!("Expected GbType::Function, got {:?}", res);
    };
    let actual = ptr.execute(&mut i.strategy, &[GbType::Integer(3), GbType::Integer(4)]);
    assert_eq!(actual, GbType::Integer(7));
}

#[test]
#[traced_test]
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
#[traced_test]
fn function_call() {
    let input = "fn foo(x, y) { x + y } foo(3, 4)";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(7));
}

#[test]
#[traced_test]
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
#[traced_test]
fn conditional_return() {
    let input =
        "fn foo(n) { if n == 1 { return true; } return false; } fn main() { foo(1); } main();";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Boolean(true));
}

#[test]
#[traced_test]
#[should_panic]
fn invalid_return_statements() {
    let input = "return 7;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    i.evaluate();
}

#[test]
#[traced_test]
fn assignment() {
    let input = "let x = 7; x = 8; x";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(8));
}

#[test]
#[traced_test]
#[should_panic]
fn bad_assignment() {
    let input = "x = 7;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    i.evaluate();
}

#[test]
#[traced_test]
#[should_panic]
fn variable_use_before_declaration() {
    let input = "x";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    i.evaluate();
}

#[test]
#[traced_test]
fn while_expression() {
    let input = "let x = 0; while x < 10 { x = x + 1 }; x ";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(10));
}

#[test]
#[traced_test]
#[should_panic]
fn disallow_mutating_functions() {
    let input = "fn foo() {} foo = 7;";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    i.evaluate();
}

#[test]
#[traced_test]
fn auto_exec_global_main() {
    let input = "fn main() { 7 }";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(7));
}

#[test]
#[traced_test]
fn recursion() {
    let input =
        "fn main() { foo(3) } fn foo(n) { if n <= 1 { return 1; } print(n); return foo(n-1); }";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(1));
}

#[test]
#[traced_test]
fn fn_call_as_expr() {
    let input = "fn foo(n) { return n; } fn main() { return foo(3) + foo(5); }";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(8));
}

#[test]
#[traced_test]
fn recursion_2() {
    let input =
        "fn foo(n) { if n <= 1 { return 1; } return foo(n-1) + 1; } fn main() { return foo(3); }";
    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(3));
}

#[test]
#[traced_test]
fn recursion_fib() {
    let input = r#"
    fn fib(n) {
        if n == 1 {
            return 1;
        } else if n <= 0 {
            return 0;
        }

        return fib(n-1) + fib(n-2);
    }

    fn main() {
        return fib(10);
    }"#;

    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    let res = i.evaluate();
    assert_eq!(res, GbType::Integer(55));
}

#[test]
#[traced_test]
#[should_panic]
fn fn_stack_pop() {
    let input = r#"
    fn foo(n) {
        return n < 1;
    }

    fn main() {
        foo(5);
        foo(n-1);
    }
    "#;

    let mut i = Interpreter::new(TreeWalking::default(), input.to_string()).unwrap();
    i.evaluate();
}

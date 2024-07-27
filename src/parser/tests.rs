#![allow(unused_imports)]

use std::ops::Deref;

use crate::{
    ast::{Alternative, Expression, IntoExpression, Node, Statement},
    lexer::Lexer,
    parser::{error::DefaultErrorHandler, Parser},
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
fn test_string_literal(exp: Expression, expected: &str) {
    if let Expression::StringLiteral(ref lit) = exp {
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
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
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
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
        parser.check_parser_errors();

        let children = ast.into_program().statements;
        assert_eq!(children.len(), 1);

        let Node::Statement(Statement::ReturnStatement(ref stmt)) = children[0] else {
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
        Box::new(DefaultErrorHandler {
            input: std::str::from_utf8(input).unwrap().to_string(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();

    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
        panic!("Expected ExpressionStatement, got {:?}", children[0]);
    };

    test_identifier((*stmt.expression).clone(), "foobar");
}

#[test]
fn integer_literal_expression() {
    let input = "5;".as_bytes();
    let mut parser = Parser::new(
        Lexer::from(input),
        Box::new(DefaultErrorHandler {
            input: std::str::from_utf8(input).unwrap().to_owned(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();

    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
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
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
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
fn string_literal_expression() {
    let input: Vec<(&str, &str)> = vec![("'hello';", "hello"), ("\"hello\";", "hello")];
    for (inp, expected) in input {
        let mut parser = Parser::new(
            Lexer::from(inp.as_bytes()),
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
        parser.check_parser_errors();

        let children = ast.into_program().statements;
        assert_eq!(children.len(), 1);

        let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0]
        else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };
        test_string_literal((*stmt.expression).clone(), expected);
    }
}
#[test]
fn prefix_expression_1() {
    let input: Vec<(&str, &str, i64)> = vec![("!5;", "!", 5), ("-15;", "-", 15)];

    for (inp, op, int) in input {
        let mut parser = Parser::new(
            Lexer::from(inp.as_bytes()),
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
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
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
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
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
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
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
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
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
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
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
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
        Box::new(DefaultErrorHandler {
            input: std::str::from_utf8(input).unwrap().to_owned(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();

    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
        panic!("Expected ExpressionStatement, got {:?}", children[0]);
    };
    test_boolean_literal((*stmt.expression).clone(), true);
}

#[test]
fn if_expression() {
    let input = "if x < y { x }";
    let mut parser = Parser::new(
        Lexer::from(input.as_bytes()),
        Box::new(DefaultErrorHandler {
            input: input.to_string(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();

    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
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
    let mut parser = Parser::new(
        Lexer::from(input.as_bytes()),
        Box::new(DefaultErrorHandler {
            input: input.to_string(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();

    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
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
    let Alternative::BlockStatement(ref bs) = if_expr.alternative else {
        panic!("Expected block statement, got {:?}", if_expr.alternative)
    };

    let Statement::ExpressionStatement(ref expr_stmt) = *bs.statements[0] else {
        panic!("Expected ExpressionStatement, got {:?}", *bs.statements[0]);
    };

    test_identifier((*expr_stmt.expression).clone(), "y");
}

#[test]
fn if_else_continuation() {
    let input = "if x < y { x } else if x < y { y }";
    let mut parser = Parser::new(
        Lexer::from(input.as_bytes()),
        Box::new(DefaultErrorHandler {
            input: input.to_string(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();

    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
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
    let Alternative::IfExpression(ref second_cond) = if_expr.alternative else {
        panic!(
            "Expected continuation condition, got {:?}",
            if_expr.alternative
        )
    };

    let Expression::IfExpression(ref ie) = **second_cond else {
        panic!("Expected if expression, got {:?}", second_cond)
    };

    let Statement::ExpressionStatement(ref expr_stmt) = *ie.consequence.statements[0]
    else {
        panic!(
            "Expected ExpressionStatement, got {:?}",
            *ie.consequence.statements[0]
        );
    };

    test_identifier((*expr_stmt.expression).clone(), "y");
}

#[test]
fn function_literal() {
    let input = "fn(x, y) { x + y; }";

    let mut parser = Parser::new(
        Lexer::from(input.as_bytes()),
        Box::new(DefaultErrorHandler {
            input: input.to_string(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();

    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
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

    let Statement::ExpressionStatement(ref expr_stmt) = *func.body.statements[0] else {
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
            Box::new(DefaultErrorHandler {
                input: inp.to_string(),
            }),
            false,
        );
        let ast = parser.parse().unwrap();
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

    let mut parser = Parser::new(
        Lexer::from(input.as_bytes()),
        Box::new(DefaultErrorHandler {
            input: input.to_string(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();

    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
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
    let mut parser = Parser::new(
        Lexer::from(input.as_bytes()),
        Box::new(DefaultErrorHandler {
            input: input.to_string(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();

    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::FunctionLiteralStatement(ref func)) = children[0]
    else {
        panic!("Expected ExpressionStatement, got {:?}", children[0]);
    };

    test_identifier(func.identifier.clone().into_expression(), "main");

    if func.literal.parameters.len() != 2 {
        panic!("Expected 2 parameters, got {:?}", func.literal.parameters);
    }

    let expected_params = ["x", "y"];
    for (i, param) in func.literal.parameters.iter().enumerate() {
        assert_eq!(param.value(), expected_params[i]);
    }

    let Statement::ExpressionStatement(ref expr_stmt) =
        *func.literal.body.statements[0]
    else {
        panic!(
            "Expected ExpressionStatement, got {:?}",
            func.literal.body.statements[0]
        );
    };

    test_infix_expression((*expr_stmt.expression).clone(), "x", "+", "y");
}

#[test]
fn while_loop() {
    let input = "while x < y { z }";
    let mut parser = Parser::new(
        Lexer::from(input.as_bytes()),
        Box::new(DefaultErrorHandler {
            input: input.to_string(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();

    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref expr_stmt)) = children[0] else {
        panic!("Expected ExpressionStatement, got {:#?}", children[0]);
    };

    let Expression::WhileExpression(ref expr) = *expr_stmt.expression  else {
        panic!("Expected WhileExpression, got {:#?}", expr_stmt.expression);
    };

    test_infix_expression(expr.condition.deref().clone(), "x", "<", "y");

    let Statement::ExpressionStatement(ref expr) = *expr.body.statements[0] else {
        panic!(
            "Expected ExpressionStatement, got {:?}",
            *expr.body.statements[0]
        );
    };

    test_identifier(expr.expression.deref().clone(), "z");
}
 #[test]
fn assignment() {
    let input = "x = 7;";
    let mut parser = Parser::new(
        Lexer::from(input.as_bytes()),
        Box::new(DefaultErrorHandler {
            input: input.to_string(),
        }),
        false,
    );
    let ast = parser.parse().unwrap();
    parser.check_parser_errors();


    let children = ast.into_program().statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref expr_stmt)) = children[0] else {
        panic!("Expected ExpressionStatement, got {:#?}", children[0]);
    };

    let Expression::InfixExpression(ref assign) = *expr_stmt.expression else { 
        panic!("Expected InfixExpression, got {:?}", expr_stmt);
    };

    test_identifier(assign.left.deref().clone(), "x");

    let Expression::IntegerLiteral(ref int) = *assign.right else {
        panic!("Expected IntegerLiteral, got {:?}", assign.right);
    };
    assert_eq!(int.value, 7);
}

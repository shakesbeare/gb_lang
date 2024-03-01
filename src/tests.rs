#![allow(unused_imports)]

#[cfg(test)]
use std::collections::HashMap;

use crate::ast::Expression;
use crate::ast::LetStatement;
use crate::ast::Node;
use crate::ast::Statement;
use crate::lexer::{LexStatus, Lexer};
use crate::parser::Parser;
use crate::token::Token;
use crate::token::TokenKind;

// **************************************
// LEXING TESTS
// **************************************
#[test]
fn identifier_basic() {
    let input = "Word".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::Identifier);
}

#[test]
fn identifier_underscore() {
    let input = "_Word".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::Identifier);
}

#[test]
fn identifier_underscore_middle() {
    let input = "my_word".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::Identifier);

    let rest = lexer.lex();
    let tok = match rest {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::Eof);
}

#[test]
fn identifier_numbers() {
    let input = "Word123".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::Identifier);
}

#[test]
fn identifier_underscore_and_numbers() {
    let input = "_Word123".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::Identifier);
}

#[test]
#[should_panic]
fn identifier_numbers_at_beginning() {
    let input = "123Word".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_ne!(tok, TokenKind::Identifier);
    assert_ne!(tok, TokenKind::FloatLiteral);
}

#[test]
#[should_panic]
fn identifier_symbol_at_end() {
    let input = "Word'".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_ne!(tok, TokenKind::Identifier);
}

#[test]
fn integer() {
    let input = "123".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::IntLiteral);
}

#[test]
fn float_hanging_decimal() {
    let input = "123.".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::FloatLiteral);
}

#[test]
fn float_balanced_decimal() {
    let input = "123.123".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::FloatLiteral);
}

#[test]
fn string_single() {
    let input = r#"'Hello'"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::StringLiteral);
}

#[test]
fn string_double() {
    let input = r#""Hello""#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::StringLiteral);
}

#[test]
fn string_single_in_double() {
    let input = r#""'hello'""#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::StringLiteral);
}

#[test]
fn string_double_in_single() {
    let input = r#"'"hello"'"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::StringLiteral);
}

#[test]
#[should_panic]
fn string_single_unclosed() {
    let input = r#"'hello"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_ne!(tok, TokenKind::StringLiteral);
}

#[test]
#[should_panic]
fn string_double_unclosed() {
    let input = r#""hello"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_ne!(tok, TokenKind::StringLiteral);
}

#[test]
#[should_panic]
fn string_single_escaped_closer() {
    let input = r#"'hello\'"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_ne!(tok, TokenKind::StringLiteral);
}

#[test]
#[should_panic]
fn string_double_escaped_closer() {
    let input = r#""hello\""#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_ne!(tok, TokenKind::StringLiteral);
}

#[test]
fn string_single_escaped_extra_closer() {
    let input = r#"'he\'llo'"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::StringLiteral);
}
#[test]
fn string_double_escaped_extra_closer() {
    let input = r#""he\"llo""#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::StringLiteral);
}

#[test]
fn operators() {
    let mut map: HashMap<&str, TokenKind> = HashMap::new();
    map.insert("+", TokenKind::OpAdd);
    map.insert("-", TokenKind::OpSub);
    map.insert("*", TokenKind::OpMul);
    map.insert("/", TokenKind::OpDiv);
    map.insert("**", TokenKind::OpExp);
    map.insert("=", TokenKind::OpAssign);
    map.insert("<", TokenKind::OpLt);
    map.insert(">", TokenKind::OpGt);

    for (k, v) in map.iter() {
        let mut lexer = Lexer::from(k.as_bytes());

        let res = lexer.lex();
        let tok = match res {
            LexStatus::Reading { token, .. } => token.kind,
            LexStatus::SyntaxError {
                failed_lexeme,
                location,
                ..
            } => {
                panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
            }
            LexStatus::Eof => TokenKind::Eof,
        };
        assert_eq!(tok, v.clone());
    }
}

#[test]
fn delimiters() {
    let mut map: HashMap<&str, TokenKind> = HashMap::new();
    map.insert("(", TokenKind::LParen);
    map.insert(")", TokenKind::RParen);
    map.insert("{", TokenKind::LBrace);
    map.insert("}", TokenKind::RBrace);
    map.insert("[", TokenKind::LBracket);
    map.insert("]", TokenKind::RBracket);
    map.insert(";", TokenKind::Semicolon);

    for (k, v) in map.iter() {
        let mut lexer = Lexer::from(k.as_bytes());

        let res = lexer.lex();
        let tok = match res {
            LexStatus::Reading { token, .. } => token.kind,
            LexStatus::SyntaxError {
                failed_lexeme,
                location,
                ..
            } => {
                panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
            }
            LexStatus::Eof => TokenKind::Eof,
        };
        assert_eq!(tok, v.clone());
    }
}

#[test]
fn single_line_comment() {
    let input = "//this is a comment".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::Eof);
}

#[test]
fn single_line_comment_2() {
    let input = r#"hello //comment
    hello"#
        .as_bytes();
    let mut lexer = Lexer::from(input);
    lexer.lex_all();

    assert_eq!(
        lexer
            .token_stream
            .iter()
            .map(|t| t.kind)
            .collect::<Vec<TokenKind>>(),
        vec![TokenKind::Identifier, TokenKind::Identifier]
    );
}

#[test]
fn block_comment() {
    let input = r#"/*this is a comment


    */"#
    .as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token.kind,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => TokenKind::Eof,
    };

    assert_eq!(tok, TokenKind::Eof);
}

#[test]
fn block_comment_2() {
    let input = r#"hello/*this is a comment


    */hello"#
        .as_bytes();
    let mut lexer = Lexer::from(input);
    lexer.lex_all();

    assert_eq!(
        lexer
            .token_stream
            .iter()
            .map(|t| t.kind)
            .collect::<Vec<TokenKind>>(),
        vec![TokenKind::Identifier, TokenKind::Identifier]
    );
}

#[test]
fn keywords() {
    let mut map: HashMap<&str, TokenKind> = HashMap::new();
    map.insert("true", TokenKind::Boolean);
    map.insert("false", TokenKind::Boolean);
    map.insert("for", TokenKind::For);
    map.insert("while", TokenKind::While);
    map.insert("let", TokenKind::Let);
    map.insert("if", TokenKind::If);
    map.insert("else", TokenKind::Else);
    // map.insert("use", TokenKind::Keyword);
    // map.insert("restrict", TokenKind::Keyword);

    for (k, v) in map.iter() {
        let mut lexer = Lexer::from(k.as_bytes());
        let res = lexer.lex();
        let tok = match res {
            LexStatus::Reading { token, .. } => token.kind,
            LexStatus::SyntaxError {
                failed_lexeme,
                location,
                ..
            } => {
                panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
            }
            LexStatus::Eof => TokenKind::Eof,
        };

        assert_eq!(&tok, v);
    }
}

// **************************************
// LEXING TESTS
// **************************************

// **************************************
// PARSING TESTS
// **************************************

#[test]
fn let_statements() {
    let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
"#;

    let mut parser = Parser::new(Lexer::from(input.as_bytes()), false);
    let ast = parser.parse();
    parser.check_parser_errors();

    let children = ast.statements;
    assert_eq!(children.len(), 3);

    let expected_identifiers = ["x", "y", "foobar"];

    for (i, child) in children.iter().enumerate() {
        let Node::Statement(Statement::LetStatement(stmt)) = child else {
            panic!("Expected LetStatement, got {:?}", child);
        };

        assert_eq!(stmt.token.kind, TokenKind::Let);
        assert_eq!(stmt.token.literal, "let");
        assert_eq!(stmt.name.value(), expected_identifiers[i]);
    }
}

#[test]
fn return_statements() {
    let input = r#"
return 5;
return 10;
return 993322;
"#;
    let mut parser = Parser::new(Lexer::from(input.as_bytes()), false);
    let ast = parser.parse();
    parser.check_parser_errors();

    let children = ast.statements;
    assert_eq!(children.len(), 3);

    for child in children {
        let Node::Statement(Statement::ReturnStatement(stmt)) = child else {
            panic!("Expected ReturnStatement, got {:?}", child);
        };

        assert_eq!(stmt.token.kind, TokenKind::Return);
        assert_eq!(stmt.token.literal, "return");
    }
}

#[test]
fn identifier_expression() {
    let input = "foobar;".as_bytes();
    let mut parser = Parser::new(Lexer::from(input), false);
    let ast = parser.parse();
    parser.check_parser_errors();

    let children = ast.statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
        panic!("Expected ExpressionStatement, got {:?}", children[0]);
    };
    let Expression::Identifier(ref ident) = *stmt.expression else {
        panic!("Expected Identifier, got {:?}", stmt.expression);
    };
    assert_eq!(ident.token.kind, TokenKind::Identifier);
    assert_eq!(ident.value(), "foobar");
    assert_eq!(ident.token.literal, "foobar");
}

#[test]
fn integer_literal_expression() {
    let input = "5;".as_bytes();
    let mut parser = Parser::new(Lexer::from(input), false);
    let ast = parser.parse();
    parser.check_parser_errors();

    let children = ast.statements;
    assert_eq!(children.len(), 1);

    let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
        panic!("Expected ExpressionStatement, got {:?}", children[0]);
    };
    let Expression::IntegerLiteral(ref lit) = *stmt.expression else {
        panic!("Expected IntegerLiteral, got {:?}", stmt.expression);
    };
    assert_eq!(lit.token.kind, TokenKind::IntLiteral);
    assert_eq!(lit.value, 5);
    assert_eq!(lit.token.literal, "5");
}

#[test]
fn prefix_expression() {
    let input: Vec<(&str, &str, i64)> = vec![("!5;", "!", 5), ("-15;", "-", 15)];


    for (inp, op, int) in input {
        let mut parser = Parser::new(Lexer::from(inp.as_bytes()), false);
        let ast = parser.parse();
        let children = ast.statements;
        parser.check_parser_errors();
        assert_eq!(children.len(), 1);
        let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };
        let Expression::PrefixExpression(ref prefix) = *stmt.expression else {
            panic!("Expected PrefixExpression, got {:?}", stmt.expression);
        };
        assert_eq!(prefix.operator, op);
        let Expression::IntegerLiteral(ref right) = *prefix.right else {
            panic!("Expected IntegerLiteral, got {:?}", prefix.right);
        };
        assert_eq!(right.value, int);
        assert_eq!(right.token.literal, int.to_string());
    }
}

#[test]
fn infix_expression() {
    let input: Vec<(&str, i64, &str, i64)> = vec![
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5),
    ];

    for (inp, left, op, right) in input {
        let mut parser = Parser::new(Lexer::from(inp.as_bytes()), false);
        let ast = parser.parse();
        let children = ast.statements;
        parser.check_parser_errors();
        assert_eq!(children.len(), 1);
        let Node::Statement(Statement::ExpressionStatement(ref stmt)) = children[0] else {
            panic!("Expected ExpressionStatement, got {:?}", children[0]);
        };

        let Expression::InfixExpression(ref infix) = *stmt.expression else {
            panic!("Expected InfixExpression, got {:?}", stmt.expression);
        };

        let Expression::IntegerLiteral(ref a_left) = *infix.left else {
            panic!("Expected IntegerLiteral, got {:?}", infix.left);
        };
        let Expression::IntegerLiteral(ref a_right) = *infix.right else {
            panic!("Expected IntegerLiteral, got {:?}", infix.right);
        };
        assert_eq!(infix.operator, op);
        assert_eq!(a_left.value, left);
        assert_eq!(a_right.value, right);
    }


}

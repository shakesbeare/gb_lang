#[cfg(test)]
use std::collections::HashMap;

#[allow(unused_imports)]
use crate::ast::AstNode;
#[allow(unused_imports)]
use crate::ast::NodeType;
#[allow(unused_imports)]
use crate::lexer::{LexStatus, Lexer};
#[allow(unused_imports)]
use crate::parser::Parser;
#[allow(unused_imports)]
use crate::token::Token;

// **************************************
// LEXING TESTS
// **************************************
#[test]
fn identifier_basic() {
    let input = "Word".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::Identifier);
}

#[test]
fn identifier_underscore() {
    let input = "_Word".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::Identifier);
}

#[test]
fn identifier_underscore_middle() {
    let input = "my_word".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::Identifier);

    let rest = lexer.lex();
    let tok = match rest {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::Eof);
}

#[test]
fn identifier_numbers() {
    let input = "Word123".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::Identifier);
}

#[test]
fn identifier_underscore_and_numbers() {
    let input = "_Word123".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::Identifier);
}

#[test]
#[should_panic]
fn identifier_numbers_at_beginning() {
    let input = "123Word".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_ne!(tok, Token::Identifier);
    assert_ne!(tok, Token::FloatLiteral);
}

#[test]
#[should_panic]
fn identifier_symbol_at_end() {
    let input = "Word'".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_ne!(tok, Token::Identifier);
}

#[test]
fn integer() {
    let input = "123".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::IntLiteral);
}

#[test]
fn float_hanging_decimal() {
    let input = "123.".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::FloatLiteral);
}

#[test]
fn float_balanced_decimal() {
    let input = "123.123".as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::FloatLiteral);
}

#[test]
fn string_single() {
    let input = r#"'Hello'"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::StringLiteral);
}

#[test]
fn string_double() {
    let input = r#""Hello""#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::StringLiteral);
}

#[test]
fn string_single_in_double() {
    let input = r#""'hello'""#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::StringLiteral);
}

#[test]
fn string_double_in_single() {
    let input = r#"'"hello"'"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::StringLiteral);
}

#[test]
#[should_panic]
fn string_single_unclosed() {
    let input = r#"'hello"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_ne!(tok, Token::StringLiteral);
}

#[test]
#[should_panic]
fn string_double_unclosed() {
    let input = r#""hello"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_ne!(tok, Token::StringLiteral);
}

#[test]
#[should_panic]
fn string_single_escaped_closer() {
    let input = r#"'hello\'"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_ne!(tok, Token::StringLiteral);
}

#[test]
#[should_panic]
fn string_double_escaped_closer() {
    let input = r#""hello\""#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_ne!(tok, Token::StringLiteral);
}

#[test]
fn string_single_escaped_extra_closer() {
    let input = r#"'he\'llo'"#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::StringLiteral);
}
#[test]
fn string_double_escaped_extra_closer() {
    let input = r#""he\"llo""#.as_bytes();
    let mut lexer = Lexer::from(input);

    let res = lexer.lex();
    let tok = match res {
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::StringLiteral);
}

#[test]
fn operators() {
    let mut map: HashMap<&str, Token> = HashMap::new();
    map.insert("+", Token::OpAdd);
    map.insert("-", Token::OpSub);
    map.insert("*", Token::OpMul);
    map.insert("/", Token::OpDiv);
    map.insert("**", Token::OpExp);
    map.insert("=", Token::OpAssign);
    map.insert("<", Token::OpLt);
    map.insert(">", Token::OpGt);

    for (k, v) in map.iter() {
        let mut lexer = Lexer::from(k.as_bytes());

        let res = lexer.lex();
        let tok = match res {
            LexStatus::Reading { token, .. } => token,
            LexStatus::SyntaxError {
                failed_lexeme,
                location,
                ..
            } => {
                panic!(
                    "Syntax Error at {:?}, lexeme: {}",
                    location, failed_lexeme
                )
            }
            LexStatus::Eof => Token::Eof,
        };
        assert_eq!(tok, v.clone());
    }
}

#[test]
fn delimiters() {
    let mut map: HashMap<&str, Token> = HashMap::new();
    map.insert("(", Token::LParen);
    map.insert(")", Token::RParen);
    map.insert("{", Token::LBrace);
    map.insert("}", Token::RBrace);
    map.insert("[", Token::LBracket);
    map.insert("]", Token::RBracket);
    map.insert(";", Token::Semicolon);

    for (k, v) in map.iter() {
        let mut lexer = Lexer::from(k.as_bytes());

        let res = lexer.lex();
        let tok = match res {
            LexStatus::Reading { token, .. } => token,
            LexStatus::SyntaxError {
                failed_lexeme,
                location,
                ..
            } => {
                panic!(
                    "Syntax Error at {:?}, lexeme: {}",
                    location, failed_lexeme
                )
            }
            LexStatus::Eof => Token::Eof,
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
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::Eof);
}

#[test]
fn single_line_comment_2() {
    let input = r#"hello //comment
    hello"#
        .as_bytes();
    let mut lexer = Lexer::from(input);
    lexer.lex_all();

    assert_eq!(
        lexer.token_stream,
        vec![Token::Identifier, Token::Identifier]
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
        LexStatus::Reading { token, .. } => token,
        LexStatus::SyntaxError {
            failed_lexeme,
            location,
            ..
        } => {
            panic!("Syntax Error at {:?}, lexeme: {}", location, failed_lexeme)
        }
        LexStatus::Eof => Token::Eof,
    };

    assert_eq!(tok, Token::Eof);
}

#[test]
fn block_comment_2() {
    let input = r#"hello/*this is a comment


    */hello"#
        .as_bytes();
    let mut lexer = Lexer::from(input);
    lexer.lex_all();

    assert_eq!(
        lexer.token_stream,
        vec![Token::Identifier, Token::Identifier]
    );
}

#[test]
fn keywords() {
    let mut map: HashMap<&str, Token> = HashMap::new();
    map.insert("true", Token::Boolean);
    map.insert("false", Token::Boolean);
    map.insert("for", Token::Keyword);
    map.insert("while", Token::Keyword);
    map.insert("let", Token::Keyword);
    map.insert("if", Token::Keyword);
    map.insert("else", Token::Keyword);
    map.insert("use", Token::Keyword);
    map.insert("restrict", Token::Keyword);

    for (k, v) in map.iter() {
        let mut lexer = Lexer::from(k.as_bytes());
        let res = lexer.lex();
        let tok = match res {
            LexStatus::Reading { token, .. } => token,
            LexStatus::SyntaxError {
                failed_lexeme,
                location,
                ..
            } => {
                panic!(
                    "Syntax Error at {:?}, lexeme: {}",
                    location, failed_lexeme
                )
            }
            LexStatus::Eof => Token::Eof,
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
fn atomic_value() {
    let input = "1234".as_bytes();
    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer, false);

    let ast = parser.parse();

    assert_eq!(
        ast.into_inner().first().unwrap().to_owned(),
        AstNode::new(NodeType::Atom, Some(Token::IntLiteral), Some("1234"))
    );
}

#[test]
fn binary_op() {
    let input = "1234 + 1234".as_bytes();
    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer, false);

    let ast = parser.parse();

    let mut expected =
        AstNode::new(NodeType::BinaryOperation, Some(Token::OpAdd), None);
    expected.children.append(&mut vec![
        AstNode::new(NodeType::Atom, Some(Token::IntLiteral), Some("1234")),
        AstNode::new(NodeType::Atom, Some(Token::IntLiteral), Some("1234")),
    ]);

    assert_eq!(ast.into_inner().first().unwrap(), &expected);
}

#[test]
fn unary_op() {
    let input = "-1234 + 1234".as_bytes();
    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer, false);

    let ast = parser.parse();
    let mut expected =
        AstNode::new(NodeType::BinaryOperation, Some(Token::OpAdd), None);
    expected.children.push(AstNode::new(
        NodeType::UnaryOperation,
        Some(Token::OpSub),
        None,
    ));
    expected.children.push(AstNode::new(
        NodeType::Atom,
        Some(Token::IntLiteral),
        Some("1234"),
    ));

    expected
        .children
        .first_mut()
        .unwrap()
        .children
        .push(AstNode::new(
            NodeType::Atom,
            Some(Token::IntLiteral),
            Some("1234"),
        ));

    assert_eq!(ast.into_inner().first().unwrap(), &expected);
}

#[test]
fn double_unary() {
    let input = "!!true".as_bytes();
    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer, false);

    let ast = parser.parse();

    let mut expected =
        AstNode::new(NodeType::UnaryOperation, Some(Token::OpBang), None);
    let mut inner =
        AstNode::new(NodeType::UnaryOperation, Some(Token::OpBang), None);

    inner.children.push(AstNode::new(
        NodeType::Atom,
        Some(Token::Boolean),
        Some("true"),
    ));
    expected.children.push(inner);

    assert_eq!(ast.into_inner().first().unwrap(), &expected);
}

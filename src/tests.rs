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
#[allow(unused_imports)]
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
    map.insert("for", TokenKind::Keyword);
    map.insert("while", TokenKind::Keyword);
    map.insert("let", TokenKind::Keyword);
    map.insert("if", TokenKind::Keyword);
    map.insert("else", TokenKind::Keyword);
    map.insert("use", TokenKind::Keyword);
    map.insert("restrict", TokenKind::Keyword);

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
fn atomic_value() {
    let input = "1234".as_bytes();
    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer, false);

    let ast = parser.parse();

    assert_eq!(
        ast.into_inner().first().unwrap().to_owned(),
        AstNode::new(
            NodeType::Atom,
            Some(Token::new("1234", TokenKind::IntLiteral, (0, 0))),
            Some("1234")
        )
    );
}

#[test]
fn binary_op() {
    let input = "1234 + 1234".as_bytes();
    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer, false);

    let ast = parser.parse();

    let mut expected = AstNode::new(
        NodeType::BinaryOperation,
        Some(Token::new("+", TokenKind::OpAdd, (0, 0))),
        None,
    );
    expected.children.append(&mut vec![
        AstNode::new(
            NodeType::Atom,
            Some(Token::new("1234", TokenKind::IntLiteral, (0, 0))),
            Some("1234"),
        ),
        AstNode::new(
            NodeType::Atom,
            Some(Token::new("1234", TokenKind::IntLiteral, (0, 0))),
            Some("1234"),
        ),
    ]);

    assert_eq!(ast.into_inner().first().unwrap(), &expected);
}

#[test]
fn unary_op() {
    let input = "-1234 + 1234".as_bytes();
    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer, false);

    let ast = parser.parse();
    let mut expected = AstNode::new(
        NodeType::BinaryOperation,
        Some(Token::new("+", TokenKind::OpAdd, (0, 0))),
        None,
    );
    expected.children.push(AstNode::new(
        NodeType::UnaryOperation,
        Some(Token::new("-", TokenKind::OpSub, (0, 0))),
        None,
    ));
    expected.children.push(AstNode::new(
        NodeType::Atom,
        Some(Token::new("1234", TokenKind::IntLiteral, (0, 0))),
        Some("1234"),
    ));

    expected
        .children
        .first_mut()
        .unwrap()
        .children
        .push(AstNode::new(
            NodeType::Atom,
            Some(Token::new("1234", TokenKind::IntLiteral, (0, 0))),
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
        AstNode::new(NodeType::UnaryOperation, Some(Token::new("!", TokenKind::OpBang, (0, 0))), None);
    let mut inner =
        AstNode::new(NodeType::UnaryOperation, Some(Token::new("!", TokenKind::OpBang, (0, 0))), None);

    inner.children.push(AstNode::new(
        NodeType::Atom,
        Some(Token::new("true", TokenKind::Boolean, (0, 0))),
        Some("true"),
    ));
    expected.children.push(inner);

    assert_eq!(ast.into_inner().first().unwrap(), &expected);
}

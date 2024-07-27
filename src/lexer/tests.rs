    #![allow(unused_imports)]

    use crate::{
        lexer::{LexStatus, Lexer},
        token::TokenKind,
    };
    use std::collections::HashMap;

    #[test]
    fn ignore_shebang() {
        let input = "#!/bin/bash\nhello";
        let mut lexer = Lexer::from(input.as_bytes());
        let res = lexer.lex();
        let tok = match res {
            LexStatus::Reading { token } => token.kind,
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
    fn lex_identifier() {
        let input: Vec<&str> = vec!["Word", "_Word", "my_word", "_Word123", "Word123"];
        for inp in input {
            let mut lexer = Lexer::from(inp.as_bytes());
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
    }

    #[test]
    #[should_panic]
    fn lex_identifier_panics() {
        let input: Vec<&str> = vec!["123Word", "Word'"];
        for inp in input {
            let mut lexer = Lexer::from(inp.as_bytes());

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
    }

    #[test]
    fn lex_integer() {
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
    fn lex_float() {
        let input: Vec<&str> = vec!["123.", "123.123"];
        for inp in input {
            let mut lexer = Lexer::from(inp.as_bytes());

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
    }

    #[test]
    fn lex_string() {
        let input: Vec<&str> = vec![
            "'Hello'",
            r#""Hello""#,
            r#""'hello'""#,
            r#"'"hello'"#,
            r#"'he\'llo'"#,
            r#""he\"llo""#,
        ];
        for inp in input {
            let mut lexer = Lexer::from(inp.as_bytes());

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
    }

    #[test]
    #[should_panic]
    fn lex_string_panics() {
        let input: Vec<&str> =
            vec!["'hello", r#""hello"#, r#""hello\""#, r#"'hello\'"#];
        for inp in input {
            let mut lexer = Lexer::from(inp.as_bytes());

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
    }

    #[test]
    fn lex_operators() {
        let mut map: HashMap<&str, TokenKind> = HashMap::new();
        map.insert("+", TokenKind::Add);
        map.insert("-", TokenKind::Subtract);
        map.insert("*", TokenKind::Multiply);
        map.insert("/", TokenKind::Divide);
        map.insert("**", TokenKind::Exponentiate);
        map.insert("=", TokenKind::Assign);
        map.insert("<", TokenKind::LessThan);
        map.insert(">", TokenKind::GreaterThan);

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
        let input: Vec<(&str, TokenKind)> = vec![
            (r#"hello //comment"#, TokenKind::Identifier),
            (r#"//this is a comment"#, TokenKind::Eof),
        ];

        for (inp, expected) in input {
            let mut lexer = Lexer::from(inp.as_bytes());

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

            assert_eq!(tok, expected);
        }
    }

    #[test]
    fn block_comment() {
        let one = r#"hello/*this is a comment


    */hello"#;
        let two = r#"/*this is a comment


    */"#;

        let input: Vec<(&str, Vec<TokenKind>)> = vec![
            (
                one,
                vec![TokenKind::Identifier, TokenKind::Identifier, TokenKind::Eof],
            ),
            (two, vec![TokenKind::Eof]),
        ];
        for (inp, expected) in input {
            let mut lexer = Lexer::from(inp.as_bytes());
            let mut toks = vec![];

            while let LexStatus::Reading { token, .. } = lexer.lex() {
                toks.push(token.kind);
            }

            if let LexStatus::Eof = lexer.lex() {
                toks.push(TokenKind::Eof);
            }

            assert_eq!(toks.len(), expected.len());
            for (i, tok) in toks.iter().enumerate() {
                assert_eq!(tok, &expected[i]);
            }
        }
    }

    #[test]
    fn keywords() {
        let mut map: HashMap<&str, TokenKind> = HashMap::new();
        map.insert("true", TokenKind::True);
        map.insert("false", TokenKind::False);
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

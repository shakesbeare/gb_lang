use crate::error::{Error, ErrorKind};

#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
enum TokenType {
    // management symbols
    EOF,
    EOL,     // this is ;
    NEWLINE, // this is \n

    // multicharacter symbols
    IDENTIFIER,
    INTEGER,
    FLOAT,

    // reserved keywords
    RETURN,
    LET,

    // operators
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    EQUALS,

    // control flow
    L_PAREN,
    R_PAREN,
    L_BRACE,
    R_BRACE,
    L_BRACKET,
    R_BRACKET,

    // other tokens
    UNKNOWN_SYMBOL,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    value: Option<String>,
}

pub fn run_lexer(input: String) -> Result<Vec<Token>, Error> {
    let mut cursor: usize = 0;
    let mut line_count: usize = 0;
    let mut column_count: usize = 0;
    let mut char_vec: Vec<char> = input.chars().collect();

    if char_vec[char_vec.len() - 1] != '\0' {
        char_vec.push('\0');
    }

    let mut tokens: Vec<Token> = Vec::<Token>::new();

    while cursor < char_vec.len() {
        // TOKENIZE

        // token is an identifer
        if char_vec[cursor].is_alphabetic() {
            let mut token: Vec<char> = Vec::<char>::new();
            token.push(char_vec[cursor]);
            cursor += 1;
            column_count += 1;

            while char_vec[cursor].is_alphanumeric() && cursor < char_vec.len() {
                token.push(char_vec[cursor]);
                cursor += 1;
                column_count += 1;
            }

            let token: String = token.into_iter().collect();

            // check if token is a known reserved keyword:
            if token == "return".to_string() {
                tokens.push(Token {
                    token_type: TokenType::RETURN,
                    value: None,
                });
            } else if token == "let".to_string() {
                tokens.push(Token {
                    token_type: TokenType::LET,
                    value: None,
                })
            }
            // otherwise, token is a user-defined identifier
            else {
                tokens.push(Token {
                    token_type: TokenType::IDENTIFIER,
                    value: Some(token.clone()),
                });
            }
        }
        // token is a number of some kind
        else if char_vec[cursor].is_numeric() || char_vec[cursor] == '.' {
            let mut token: Vec<char> = Vec::<char>::new();
            token.push(char_vec[cursor]);
            cursor += 1;
            column_count += 1;

            while cursor < char_vec.len() {
                if char_vec[cursor].is_alphabetic() {
                    let err = Error::new(
                        "",
                        line_count,
                        column_count,
                        ErrorKind::INVALID_NUMERIC_LITERAL,
                    );

                    return Err(err);
                } else if char_vec[cursor].is_numeric() || char_vec[cursor] == '.' {
                    token.push(char_vec[cursor]);
                    cursor += 1;
                    column_count += 1;
                } else {
                    break;
                }
            }

            let token: String = token.into_iter().collect();

            if token.contains('.') {
                tokens.push(Token {
                    token_type: TokenType::FLOAT,
                    value: Some(token),
                })
            } else {
                tokens.push(Token {
                    token_type: TokenType::INTEGER,
                    value: Some(token),
                })
            }
        }
        // token is a single length character
        if char_vec[cursor].is_whitespace() {
            // whitespace is ignored

            if char_vec[cursor] == '\n' {
                line_count += 1;
                column_count = 0;
            }
            cursor += 1;
            continue;
        }

        tokens.push(match char_vec[cursor] {
            '+' => Token {
                token_type: TokenType::PLUS,
                value: None,
            },
            '-' => Token {
                token_type: TokenType::MINUS,
                value: None,
            },
            '*' => Token {
                token_type: TokenType::MULTIPLY,
                value: None,
            },
            '/' => Token {
                token_type: TokenType::DIVIDE,
                value: None,
            },
            '=' => Token {
                token_type: TokenType::EQUALS,
                value: None,
            },
            ';' => Token {
                token_type: TokenType::EOL,
                value: None,
            },
            '(' => Token {
                token_type: TokenType::L_PAREN,
                value: None,
            },
            ')' => Token {
                token_type: TokenType::R_PAREN,
                value: None,
            },
            '{' => Token {
                token_type: TokenType::L_BRACE,
                value: None,
            },
            '}' => Token {
                token_type: TokenType::R_BRACE,
                value: None,
            },
            '[' => Token {
                token_type: TokenType::L_BRACKET,
                value: None,
            },
            ']' => Token {
                token_type: TokenType::R_BRACKET,
                value: None,
            },
            '\0' => Token {
                token_type: TokenType::EOF,
                value: None,
            },
            _ => Token {
                token_type: TokenType::UNKNOWN_SYMBOL,
                value: Some(char_vec[cursor].to_string()),
            },
        });

        cursor += 1;
        column_count += 1;
    }

    Ok(tokens)
}

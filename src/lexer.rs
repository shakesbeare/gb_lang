use crate::error::{Error, ErrorKind};

#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    // management symbols
    EOF,
    SEMICOLON,
    NEWLINE,

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
    COLON,

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
#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: Option<String>,
}

impl Token {
    pub fn new(token_type: TokenType, value: Option<String>) -> Self {
        Self {
            token_type: token_type,
            value: value,
        }
    }
}

pub fn run_lexer(input: String) -> Result<Vec<Token>, Error> {
    let mut cursor: usize = 0;
    let mut line_count: usize = 1;
    let mut column_count: usize = 1;
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
                tokens.push(Token::new(TokenType::RETURN, None));
            } else if token == "let".to_string() {
                tokens.push(Token::new(TokenType::LET, None));
            }
            // otherwise, token is a user-defined identifier
            else {
                tokens.push(Token::new(TokenType::IDENTIFIER, Some(token)));
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
                        "Numeric literals cannot contain alphabetic characters.",
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
                let full_stop_count = token.matches('.').count();

                if full_stop_count > 1 {
                    let err = Error::new(
                        "Numeric literals can only contain 0 or 1 decimal points",
                        line_count,
                        column_count,
                        ErrorKind::INVALID_NUMERIC_LITERAL,
                    );

                    return Err(err);
                }

                tokens.push(Token::new(TokenType::FLOAT, Some(token)));
            } else {
                tokens.push(Token::new(TokenType::INTEGER, Some(token)));
            }
        }
        // token is a single length character
        if char_vec[cursor].is_whitespace() {
            // whitespace is ignored

            if char_vec[cursor] == '\n' {
                line_count += 1;
                column_count = 1;
            }
            cursor += 1;
            continue;
        }

        tokens.push(match char_vec[cursor] {
            '+' => Token::new(TokenType::PLUS, None),
            '-' => Token::new(TokenType::MINUS, None),
            '*' => Token::new(TokenType::MULTIPLY, None),
            '/' => Token::new(TokenType::DIVIDE, None),
            '=' => Token::new(TokenType::EQUALS, None),
            ':' => Token::new(TokenType::COLON, None),
            ';' => Token::new(TokenType::SEMICOLON, None),
            '(' => Token::new(TokenType::L_PAREN, None),
            ')' => Token::new(TokenType::R_PAREN, None),
            '{' => Token::new(TokenType::L_BRACE, None),
            '}' => Token::new(TokenType::R_BRACE, None),
            '[' => Token::new(TokenType::L_BRACKET, None),
            ']' => Token::new(TokenType::R_BRACKET, None),
            '\0' => Token::new(TokenType::EOF, None),
            _ => Token::new(
                TokenType::UNKNOWN_SYMBOL,
                Some(char_vec[cursor].to_string()),
            ),
        });

        cursor += 1;
        column_count += 1;
    }

    Ok(tokens)
}

use crate::error::{Error, ErrorKind};

#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TT {
    // management symbols
    EOF,
    SEMICOLON,
    NEWLINE,

    // multicharacter symbols
    IDENTIFIER,
    INTEGER,
    FLOAT,
    STRING,

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
    pub token_type: TT,
    pub value: Option<String>,
    pub row: usize,
    pub column: usize,
}

impl Token {
    pub fn new(token_type: TT, value: Option<String>, row: usize, column: usize) -> Self {
        Self {
            token_type: token_type,
            value: value,
            row: row,
            column: column,
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
                tokens.push(Token::new(TT::RETURN, None, line_count, column_count));
            } else if token == "let".to_string() {
                tokens.push(Token::new(TT::LET, None, line_count, column_count));
            }
            // otherwise, token is a user-defined identifier
            else {
                tokens.push(Token::new(
                    TT::IDENTIFIER,
                    Some(token),
                    line_count,
                    column_count,
                ));
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

                tokens.push(Token::new(TT::FLOAT, Some(token), line_count, column_count));
            } else {
                tokens.push(Token::new(
                    TT::INTEGER,
                    Some(token),
                    line_count,
                    column_count,
                ));
            }
        }
        // token is the start of a string literal
        else if char_vec[cursor] == '"' || char_vec[cursor] == '\'' {
            let quote_type = char_vec[cursor];
            let mut token: Vec<char> = Vec::<char>::new();

            token.push(char_vec[cursor]);
            cursor += 1;
            column_count += 1;

            while char_vec[cursor] != quote_type && cursor < char_vec.len() {
                token.push(char_vec[cursor]);
                cursor += 1;
                column_count += 1;
            }
            token.push(char_vec[cursor]);
            cursor += 1;
            column_count += 1;

            let token: String = token.into_iter().collect();
            tokens.push(Token::new(
                TT::STRING,
                Some(token),
                line_count,
                column_count,
            ));
        }
        // this line is a comment
        else if char_vec[cursor] == '/' && char_vec[cursor + 1] == '/' {
            while char_vec[cursor] != '\n' {
                cursor += 1
            }
        }

        // Token is whitespace
        if char_vec[cursor].is_whitespace() {
            // whitespace is ignored

            if char_vec[cursor] == '\n' {
                line_count += 1;
                column_count = 1;
            }
            cursor += 1;
            continue;
        } else {
            // token is a single length character
            tokens.push(match char_vec[cursor] {
                '+' => Token::new(TT::PLUS, None, line_count, column_count),
                '-' => Token::new(TT::MINUS, None, line_count, column_count),
                '*' => Token::new(TT::MULTIPLY, None, line_count, column_count),
                '/' => Token::new(TT::DIVIDE, None, line_count, column_count),
                '=' => Token::new(TT::EQUALS, None, line_count, column_count),
                ':' => Token::new(TT::COLON, None, line_count, column_count),
                ';' => Token::new(TT::SEMICOLON, None, line_count, column_count),
                '(' => Token::new(TT::L_PAREN, None, line_count, column_count),
                ')' => Token::new(TT::R_PAREN, None, line_count, column_count),
                '{' => Token::new(TT::L_BRACE, None, line_count, column_count),
                '}' => Token::new(TT::R_BRACE, None, line_count, column_count),
                '[' => Token::new(TT::L_BRACKET, None, line_count, column_count),
                ']' => Token::new(TT::R_BRACKET, None, line_count, column_count),
                '\0' => Token::new(TT::EOF, None, line_count, column_count),
                _ => Token::new(
                    TT::UNKNOWN_SYMBOL,
                    Some(char_vec[cursor].to_string()),
                    line_count,
                    column_count,
                ),
            });

            cursor += 1;
            column_count += 1;
        }
    }

    Ok(tokens)
}

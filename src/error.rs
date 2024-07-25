use crate::token::Token;

const UP_ARROWHEAD: &str = "⌃";
const DIVIDER: &str = "--------------------------------";

pub struct ErrorHandler<'a> {
    pub input: &'a str,
}

impl<'a> ErrorHandler<'a> {
    pub fn syntax_error(&self, token: Token) -> String {
        let line = self.input.split('\n').nth(token.location.line - 1).unwrap();
        let padding = " ".repeat(token.location.col - 1);
        format!(
            "\
{DIVIDER}
Syntax Error on line {} at col {}
{DIVIDER}
 {line}
{padding}{UP_ARROWHEAD}
Unexpected {:?}",
            token.location.line, token.location.col, token.literal,
        )
    }
}

mod tests {
    #![allow(unused_imports)]

    use crate::*;

    #[test]
    fn syntax_error() {
        let input = vec![
            (
                "hello {", // input
                r#"--------------------------------
Syntax Error on line 1 at col 8
--------------------------------
 hello {
       ⌃
Unexpected "{""#,
            ),
            (
                "{", // input
                r#"--------------------------------
Syntax Error on line 1 at col 2
--------------------------------
 {
 ⌃
Unexpected "{""#,
            ),
        ];
        for (inp, expected) in input {
            let mut p = parser::Parser::new(
                lexer::Lexer::from(inp.as_bytes()),
                error::ErrorHandler { input: inp },
                false,
            );
            p.parse();

            assert_eq!(p.errors.len(), 1);
            let actual = p.errors.first().unwrap();
            assert_eq!(actual, expected);
        }
    }
}

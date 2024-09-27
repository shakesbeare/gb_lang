use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
    pub location: Point,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.literal == other.literal
    }
}

impl std::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.literal.hash(state);
    }
}

impl Eq for Token {}

impl Token {
    pub fn new<S: Into<String>, P: Into<Point>>(
        literal: S,
        kind: TokenKind,
        location: P,
    ) -> Self {
        Self {
            literal: literal.into(),
            kind,
            location: location.into(),
        }
    }

    #[allow(dead_code)]
    pub fn eof() -> Self {
        Self {
            literal: "\0".to_string(),
            kind: TokenKind::Eof,
            location: (0, 0).into(),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum TokenKind {
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    Identifier,
    True,
    False,

    If,
    For,
    While,
    Let,
    Else,
    Fn,
    Return,

    Add,
    Subtract,
    Multiply,
    Divide,
    Exponentiate,
    Assign,
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterEquals,
    LessEquals,
    Bang,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Comma,
    Semicolon,
    Eol,
    Eof,
}

impl From<&String> for TokenKind {
    fn from(val: &String) -> Self {
        if val == "let" {
            return TokenKind::Let;
        } else if val == "if" {
            return TokenKind::If;
        } else if val == "for" {
            return TokenKind::For;
        } else if val == "while" {
            return TokenKind::While;
        } else if val == "else" {
            return TokenKind::Else;
        } else if val == "fn" {
            return TokenKind::Fn;
        } else if val == "return" {
            return TokenKind::Return;
        } else if val == "false" {
            return TokenKind::False;
        } else if val == "true" {
            return TokenKind::True;
        } else {
            return TokenKind::Identifier;
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Point {
    pub line: usize,
    pub col: usize,
}

impl From<(usize, usize)> for Point {
    fn from(value: (usize, usize)) -> Self {
        Point {
            line: value.0,
            col: value.1,
        }
    }
}

pub trait HasKind {
    fn has_kind(&self, kind: TokenKind) -> bool;
}

impl HasKind for Option<Token> {
    fn has_kind(&self, kind: TokenKind) -> bool {
        self.as_ref().is_some_and(|t| t.kind == kind)
    }
}

impl HasKind for Rc<Token> {
    fn has_kind(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }
}

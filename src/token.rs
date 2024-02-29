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
    pub fn new<S: Into<String>, P: Into<Point>>(literal: S, kind: TokenKind, location: P) -> Self {
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
    Boolean,

    Keyword,

    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpExp,
    OpAssign,
    OpGt,
    OpLt,
    OpBang,

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

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Point {
    line: usize,
    col: usize,
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
        match self {
            Some(v) => v.kind == kind,
            None => false,
        }
    }
}



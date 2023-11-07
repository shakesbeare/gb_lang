#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Token {
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

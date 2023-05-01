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

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Semicolon,
    Eol,
    Eof,
}

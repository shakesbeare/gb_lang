#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Token {
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    Identifier,

    OpAdd,
    OpSub,
    OpMul,
    OpDiv,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
}

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
    OpAssign,
    OpGt,
    OpLt,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    EOL,
    EOF,
}

#[derive(Eq, PartialEq, Hash, Clone)]
pub enum Token {
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    Identifier,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
}

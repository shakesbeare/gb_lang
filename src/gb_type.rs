use crate::ast::Expr;
use std::{fmt, mem::Discriminant};

pub fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

#[derive(Clone, Debug, PartialEq)]
pub enum GbType {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Function(Expr),
    None,
}

impl GbType {
    fn discriminant(&self) -> Discriminant<GbType> {
        std::mem::discriminant(self)
    }
}

impl PartialOrd for GbType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if !variant_eq(self, other) {
            return None;
        }

        let is_less = match *self {
            GbType::Integer(x) => match *other {
                GbType::Integer(y) => x < y,
                GbType::Float(y) => (x as f64) < y,
                _ => todo!(),
            },
            GbType::Float(x) => match *other {
                GbType::Float(y) => x < y,
                GbType::Integer(y) => x < y as f64,
                _ => todo!(),
            },
            _ => todo!(),
        };

        if is_less {
            return Some(std::cmp::Ordering::Less);
        } else {
            return Some(std::cmp::Ordering::Greater);
        }
    }
}

impl std::ops::Add<GbType> for GbType {
    type Output = GbType;

    fn add(self, rhs: GbType) -> GbType {
        match self {
            GbType::Integer(x) => match rhs {
                GbType::Integer(y) => GbType::Integer(x + y),
                GbType::Float(y) => GbType::Float(x as f64 + y),
                _ => todo!(),
            },
            GbType::Float(x) => match rhs {
                GbType::Float(y) => GbType::Float(x + y),
                GbType::Integer(y) => GbType::Float(x + y as f64),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl std::ops::Sub<GbType> for GbType {
    type Output = GbType;

    fn sub(self, rhs: GbType) -> GbType {
        match self {
            GbType::Integer(x) => match rhs {
                GbType::Integer(y) => GbType::Integer(x - y),
                GbType::Float(y) => GbType::Float(x as f64 - y),
                _ => todo!(),
            },
            GbType::Float(x) => match rhs {
                GbType::Float(y) => GbType::Float(x - y),
                GbType::Integer(y) => GbType::Float(x - y as f64),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl std::ops::Mul<GbType> for GbType {
    type Output = GbType;

    fn mul(self, rhs: GbType) -> GbType {
        match self {
            GbType::Integer(x) => match rhs {
                GbType::Integer(y) => GbType::Integer(x * y),
                GbType::Float(y) => GbType::Float(x as f64 * y),
                _ => todo!(),
            },
            GbType::Float(x) => match rhs {
                GbType::Float(y) => GbType::Float(x * y),
                GbType::Integer(y) => GbType::Float(x * y as f64),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl std::ops::Div<GbType> for GbType {
    type Output = GbType;

    fn div(self, rhs: GbType) -> GbType {
        match self {
            GbType::Integer(x) => match rhs {
                GbType::Integer(y) => GbType::Integer(x / y),
                GbType::Float(y) => GbType::Float(x as f64 / y),
                _ => todo!(),
            },
            GbType::Float(x) => match rhs {
                GbType::Float(y) => GbType::Float(x / y),
                GbType::Integer(y) => GbType::Float(x / y as f64),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl std::fmt::Display for GbType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                GbType::Integer(x) => x.to_string(),
                GbType::Float(x) => x.to_string(),
                GbType::String(x) => format!("\" {} \"", x).to_string(),
                GbType::Boolean(x) => x.to_string(),
                GbType::Function(x) => format!("Function: {:?}", x).to_string(),
                GbType::None => "".to_string(),
            }
        )
    }
}

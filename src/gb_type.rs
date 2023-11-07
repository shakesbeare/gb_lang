use std::{fmt, ops::Not};

pub fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

pub fn gb_pow(left: GbType, right: GbType) -> GbType {
    match left {
        GbType::Integer(x) => {
            match right {
                GbType::Integer(y) => GbType::Integer(i64::pow(x, y as u32)),
                GbType::Float(y) => GbType::Float(f64::powf(x as f64, y )),
                _ => GbType::Error
            }
        }
        GbType::Float(x) => {
            match right {
                GbType::Integer(y) => GbType::Float(f64::powf(x, y as f64)),
                GbType::Float(y) => GbType::Float(f64::powf(x, y)),
                _ => GbType::Error
            }
        },
        _ => GbType::Error,
    }
}

/// Converts an arbitrary GbType to GbType::Boolean
pub fn gb_bool(x: GbType) -> GbType {
    match x {
        GbType::Error => GbType::Boolean(false),
        GbType::Integer(x) => {
            if x == 0 {
                GbType::Boolean(false)
            } else {
                GbType::Boolean(true)
            }
        },
        GbType::Float(x) => {
            if x == 0.0 {
                GbType::Boolean(false)
            } else {
                GbType::Boolean(true)
            }
        } ,
        GbType::Boolean(x) => GbType::Boolean(x),
        GbType::String(x) => {
            if x == *"" {
                GbType::Boolean(false)
            } else {
                GbType::Boolean(true)
            }
        },
        GbType::None => GbType::Boolean(false),
        _ => unreachable!(),
    }
}

/// Returns a string representation of the type of the input
pub fn gb_type_of(x: GbType) -> String {
    match x {
        GbType::Empty => "Empty",
        GbType::Error => "Error",
        GbType::None => "None",
        GbType::Integer(_) => "Integer",
        GbType::Float(_) => "Float",
        GbType::Boolean(_) => "Boolean",
        GbType::String(_) => "String",
    }.into()
}

#[derive(Debug, Clone, PartialEq)]
pub enum GbType {
    Empty,
    Error,
    None,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
}

impl Not for GbType {
    type Output = GbType;

    fn not(self) -> GbType {
        let bool_ = gb_bool(self);
        match bool_ {
            GbType::Boolean(x) => GbType::Boolean(!x),
            _ => unreachable!(),
        }
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
                _ => todo!()
            }
        )
    }
}

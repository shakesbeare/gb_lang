use super::InterpreterStrategy;
use crate::{
    ast::{FunctionLiteral, IntoNode},
    token::Token,
};
use std::{borrow::Borrow, collections::HashMap, fmt, rc::Rc};
use tracing::instrument;

pub trait GbFunc: std::fmt::Debug {
    fn execute(
        &self,
        strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
        token: Token,
    ) -> GbType;
}

impl GbFunc for FunctionLiteral {
    // TODO: this really should execute on a pointer to the block
    //       rather than cloning the block...
    fn execute(
        &self,
        strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
        _token: Token,
    ) -> GbType {
        strategy.push_env();
        let new_env = strategy.top_env();
        for (param, arg) in self.parameters.iter().zip(args) {
            new_env.insert(param.value(), arg.clone());
        }
        let val = strategy.eval(self.body.clone().into_node().borrow(), true, true);
        strategy.pop_env();
        val
    }
}

#[derive(Debug, Clone)]
pub enum GbType {
    Empty,
    /// Represents an error that has occurred
    Error(Token, GbError),
    None,
    /// Exits the program when returned. This should only be accessible through `std.exit()` or
    /// `exit()`
    ExitSignal(i32),
    /// Represents the name of an object
    Name(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Function(Rc<dyn GbFunc>),
    Namespace(HashMap<String, Rc<GbType>>),
    ReturnValue(Box<GbType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum GbError {
    MisplacedReturn,
    InvalidOperatorForTypes {
        left: String,
        right: String,
        op: String,
    },
    InvalidOperatorForType {
        operand: String,
        operator: String,
    },
    WrongTypeInFunctionArg,
    FailedToResolveNameLookup,
    ConditionalMustEvaluateToBool,
    VariableUsedBeforeDeclaration,
    VariableCannotBeAssignedToType,
    FunctionMayNotBeMutated,
    DotLookupOnlyApplicableToIdentifiers,
    AttemptedToCallNonFunctionType,
}

impl fmt::Display for GbError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GbError::MisplacedReturn => write!(
                f,
                "Misplaced Return: Return is only allowed within a function"
            ),
            GbError::InvalidOperatorForTypes { left, right, op } => write!(
                f,
                "Invalid Operator: Cannot perform \"{}\" between {} and {}",
                op, left, right
            ),
            GbError::InvalidOperatorForType { operand, operator } => write!(
                f,
                "Invalid Operator: Cannot perform \"{}\" on {}",
                operator, operand,
            ),
            GbError::WrongTypeInFunctionArg => {
                write!(f, "Wrong type in function argument")
            }
            GbError::FailedToResolveNameLookup => {
                write!(f, "Failed to resolve name lookup")
            }
            GbError::ConditionalMustEvaluateToBool => {
                write!(f, "Conditional must evaluate to bool")
            }
            GbError::VariableUsedBeforeDeclaration => {
                write!(f, "Variable used before declaration")
            }
            GbError::VariableCannotBeAssignedToType => {
                write!(f, "Variable cannot be assigned to type")
            }
            GbError::FunctionMayNotBeMutated => {
                write!(f, "Function types may not be mutated")
            }
            GbError::DotLookupOnlyApplicableToIdentifiers => {
                write!(f, "Dot lookup only applicable to identifiers")
            }
            GbError::AttemptedToCallNonFunctionType => {
                write!(f, "Attempted to call non function type")
            }
        }
    }
}

impl GbType {
    #[instrument]
    pub fn unwrap_return(self) -> Self {
        let mut x = self;
        while let GbType::ReturnValue(inner) = x {
            tracing::trace!("Unwrapping return");
            x = (*inner).unwrap_return();
        }

        if let GbType::ExitSignal(i) = x {
            std::process::exit(i)
        }

        x
    }

    pub fn get_attr(&self, attr: impl AsRef<str>) -> Option<Rc<GbType>> {
        match self {
            GbType::Namespace(ns) => {
                // TODO remove unwrap
                Some(ns.get(attr.as_ref())?.clone())
            }
            _ => todo!(),
        }
    }

    pub fn is_gb_none(&self) -> bool {
        matches!(self, GbType::None | GbType::Empty)
    }
}

pub fn gb_eq(left: GbType, right: GbType) -> Result<bool, GbError> {
    // function types should never evaluate as equal to each other
    match (&left, &right) {
        (GbType::Integer(l), GbType::Integer(r)) => Ok(l.eq(r)),
        (GbType::Name(l), GbType::Name(r)) => Ok(l.eq(r)),
        (GbType::Float(l), GbType::Float(r)) => Ok(l.eq(r)),
        (GbType::String(l), GbType::String(r)) => Ok(l.eq(r)),
        (GbType::Boolean(l), GbType::Boolean(r)) => Ok(l.eq(r)),
        (GbType::None, GbType::None) => Ok(true),
        (GbType::Empty, GbType::Empty) => Ok(true),
        (GbType::Error(_, _), GbType::Error(_, _)) => Ok(false),
        _ => Err(GbError::InvalidOperatorForTypes {
            left: left.to_string(),
            right: right.to_string(),
            op: String::from("=="),
        }),
    }
}

pub fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

pub fn gb_pow(left: GbType, right: GbType) -> Result<GbType, GbError> {
    match (&left, &right) {
        (GbType::Integer(x), GbType::Integer(y)) => {
            Ok(GbType::Integer(i64::pow(*x, *y as u32)))
        }
        (GbType::Integer(x), GbType::Float(y)) => {
            Ok(GbType::Float(f64::powf(*x as f64, *y)))
        }
        (GbType::Float(x), GbType::Float(y)) => Ok(GbType::Float(f64::powf(*x, *y))),
        (GbType::Float(x), GbType::Integer(y)) => {
            Ok(GbType::Float(f64::powf(*x, *y as f64)))
        }
        _ => Err(GbError::InvalidOperatorForTypes {
            left: left.to_string(),
            right: right.to_string(),
            op: String::from("**"),
        }),
    }
}

/// Converts an arbitrary GbType to GbType::Boolean
pub fn gb_bool(x: GbType) -> GbType {
    match x {
        GbType::Integer(x) => {
            if x == 0 {
                GbType::Boolean(false)
            } else {
                GbType::Boolean(true)
            }
        }
        GbType::Float(x) => {
            if x == 0.0 {
                GbType::Boolean(false)
            } else {
                GbType::Boolean(true)
            }
        }
        GbType::Boolean(x) => GbType::Boolean(x),
        GbType::String(x) => {
            if x == *"" {
                GbType::Boolean(false)
            } else {
                GbType::Boolean(true)
            }
        }
        _ => GbType::Boolean(false),
    }
}

/// Returns a string representation of the type of the input
pub fn gb_type_of(x: impl std::ops::Deref<Target = GbType>) -> String {
    match *x {
        GbType::Empty => "Empty",
        // TODO: Custom type representations for different errors
        GbType::Error(_, _) => "Error",
        GbType::None => "None",
        GbType::Integer(_) => "Integer",
        GbType::Float(_) => "Float",
        GbType::Boolean(_) => "Boolean",
        GbType::String(_) => "String",
        GbType::Name(_) => "Name",
        GbType::Function(_) => "Function",
        GbType::ReturnValue(_) => "Return Value",
        GbType::Namespace(_) => "Namespace",
        GbType::ExitSignal(_) => "ExitSignal",
    }
    .into()
}

pub fn gb_not(operand: GbType) -> GbType {
    let bool_ = gb_bool(operand);
    match bool_ {
        GbType::Boolean(x) => GbType::Boolean(!x),
        _ => unreachable!(),
    }
}

pub fn gb_cmp(
    left: GbType,
    right: GbType,
) -> Result<Option<std::cmp::Ordering>, GbError> {
    match (&left, &right) {
        (GbType::Integer(x), GbType::Integer(y)) => Ok(x.partial_cmp(y)),
        (GbType::Integer(x), GbType::Float(y)) => {
            let x = *x as f64;
            Ok(x.partial_cmp(y))
        }
        (GbType::Float(x), GbType::Float(y)) => Ok(x.partial_cmp(y)),
        (GbType::Float(x), GbType::Integer(y)) => {
            let y = *y as f64;
            Ok(x.partial_cmp(&y))
        }
        _ => Err(GbError::InvalidOperatorForTypes {
            left: left.to_string(),
            right: right.to_string(),
            op: String::from("<>"),
        }),
    }
}

pub fn gb_add(left: GbType, right: GbType) -> Result<GbType, GbError> {
    match (&left, &right) {
        (GbType::Integer(x), GbType::Integer(y)) => Ok(GbType::Integer(x + y)),
        (GbType::Integer(x), GbType::Float(y)) => Ok(GbType::Float(*x as f64 + y)),
        (GbType::Float(x), GbType::Integer(y)) => Ok(GbType::Float(x + *y as f64)),
        (GbType::Float(x), GbType::Float(y)) => Ok(GbType::Float(x + y)),
        (GbType::String(x), GbType::String(y)) => Ok({
            let mut x = x.clone();
            x.push_str(y);
            GbType::String(x)
        }),
        _ => Err(GbError::InvalidOperatorForTypes {
            left: left.to_string(),
            right: right.to_string(),
            op: String::from("+"),
        }),
    }
}

pub fn gb_sub(left: GbType, right: GbType) -> Result<GbType, GbError> {
    match (&left, &right) {
        (GbType::Integer(x), GbType::Integer(y)) => Ok(GbType::Integer(x - y)),
        (GbType::Integer(x), GbType::Float(y)) => Ok(GbType::Float(*x as f64 - y)),
        (GbType::Float(x), GbType::Float(y)) => Ok(GbType::Float(x - y)),
        (GbType::Float(x), GbType::Integer(y)) => Ok(GbType::Float(x - *y as f64)),
        _ => Err(GbError::InvalidOperatorForTypes {
            left: left.to_string(),
            right: right.to_string(),
            op: String::from("-"),
        }),
    }
}

pub fn gb_mul(left: GbType, right: GbType) -> Result<GbType, GbError> {
    match (&left, &right) {
        (GbType::Integer(x), GbType::Integer(y)) => Ok(GbType::Integer(x * y)),
        (GbType::Integer(x), GbType::Float(y)) => Ok(GbType::Float(*x as f64 * y)),
        (GbType::Float(x), GbType::Integer(y)) => Ok(GbType::Float(x * *y as f64)),
        (GbType::Float(x), GbType::Float(y)) => Ok(GbType::Float(x * y)),
        _ => Err(GbError::InvalidOperatorForTypes {
            left: left.to_string(),
            right: right.to_string(),
            op: String::from("*"),
        }),
    }
}

pub fn gb_div(left: GbType, right: GbType) -> Result<GbType, GbError> {
    match (&left, &right) {
        (GbType::Integer(x), GbType::Integer(y)) => Ok(GbType::Integer(x / y)),
        (GbType::Integer(x), GbType::Float(y)) => Ok(GbType::Float(*x as f64 / y)),
        (GbType::Float(x), GbType::Integer(y)) => Ok(GbType::Float(x / *y as f64)),
        (GbType::Float(x), GbType::Float(y)) => Ok(GbType::Float(x / y)),
        _ => Err(GbError::InvalidOperatorForTypes {
            left: left.to_string(),
            right: right.to_string(),
            op: String::from("/"),
        }),
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
                GbType::String(x) => x.to_string(),
                GbType::Boolean(x) => x.to_string(),
                GbType::None => "None".to_string(),
                GbType::Function(x) => format!("Function Object: {:?}", x),
                GbType::Error(_tok, e) => format!("{:?}", e),
                GbType::Name(x) => x.to_string(),
                i => {
                    format!("No string formatter for {:?}", i)
                }
            }
        )
    }
}

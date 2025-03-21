use std::rc::Rc;

use crate::token::Token;

use super::{
    environment::Environment,
    gb_type::{GbError, GbFunc, GbType},
    GbErrorKind, InterpreterStrategy,
};

#[derive(Debug, Clone, PartialEq)]
pub(super) struct GbPrint {}

impl GbPrint {
    /// Output a value to stdout at runtime
    pub(super) fn print(&self, args: &[GbType]) {
        for arg in args {
            print!("{} ", arg);
        }
        println!();
    }

    pub(super) fn export(self) -> GbType {
        GbType::Function(Rc::new(self), None)
    }
}

impl GbFunc for GbPrint {
    fn execute(
        &self,
        _strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
        _token: Token,
        _env: Option<Environment>,
    ) -> Result<GbType, GbError> {
        self.print(args);
        Ok(GbType::None)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct GbToString {}

impl GbToString {
    pub(super) fn to_string(&self, obj: &GbType) -> GbType {
        GbType::String(obj.to_string())
    }

    pub(super) fn export(self) -> GbType {
        GbType::Function(Rc::new(self), None)
    }
}

impl GbFunc for GbToString {
    fn execute(
        &self,
        _strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
        _token: Token,
        _env: Option<Environment>,
    ) -> Result<GbType, GbError> {
        if args.len() > 1 {
            panic!("Too many arguments");
        }
        Ok(self.to_string(&args[0]))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct GbDump {}

impl GbDump {
    /// Dumps the entire contents of the call stack and writes it to stdout
    /// This will probably be very slow
    pub(super) fn dump(&self, strategy: &mut dyn InterpreterStrategy) -> GbType {
        println!("{:#?}", strategy.stack());
        GbType::None
    }

    pub(super) fn export(self) -> GbType {
        GbType::Function(Rc::new(self), None)
    }
}

impl GbFunc for GbDump {
    fn execute(
        &self,
        strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
        _token: Token,
        _env: Option<Environment>,
    ) -> Result<GbType, GbError> {
        if !args.is_empty() {
            panic!("Too many arguments");
        }
        Ok(self.dump(strategy))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct GbExit {}

impl GbExit {
    /// Exits the program gracefully
    /// Panics if the provided exit code is not representable as an i32
    pub(super) fn exit(&self, token: Token, code: GbType) -> Result<GbType, GbError> {
        match code {
            GbType::Integer(i) => Ok(GbType::ReturnValue(Box::new(GbType::ExitSignal(i as i32)))),
            _ => Err(GbError {
                token: Some(token),
                kind: GbErrorKind::WrongTypeInFunctionArg,
            }),
        }
    }

    pub(super) fn export(self) -> GbType {
        GbType::Function(Rc::new(self), None)
    }
}

impl GbFunc for GbExit {
    fn execute(
        &self,
        _strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
        token: Token,
        _env: Option<Environment>,
    ) -> Result<GbType, GbError> {
        if args.len() > 1 {
            panic!("Too many arguments");
        } else if args.is_empty() {
            self.exit(token, GbType::Integer(0))
        } else {
            self.exit(token, args[0].clone())
        }
    }
}

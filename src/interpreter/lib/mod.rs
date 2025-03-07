use std::rc::Rc;

use super::{
    gb_type::{GbError, GbFunc, GbType},
    InterpreterStrategy,
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
        GbType::Function(Rc::new(self))
    }
}

impl GbFunc for GbPrint {
    fn execute(
        &self,
        _strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
    ) -> GbType {
        self.print(args);
        GbType::None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct GbWarn {}

impl GbWarn {
    /// Generate a warning.
    /// This may result in an output sooner in the execution process than `GbPrint`.
    pub(super) fn warn(&self, info: impl AsRef<str>) {
        tracing::warn!("{}", info.as_ref());
    }

    pub(super) fn export(self) -> GbType {
        GbType::Function(Rc::new(self))
    }
}

impl GbFunc for GbWarn {
    fn execute(
        &self,
        _strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
    ) -> GbType {
        if args.len() > 1 {
            panic!("Too many arguments");
        }
        self.warn(args[0].to_string());
        GbType::None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct GbToString {}

impl GbToString {
    pub(super) fn to_string(&self, obj: &GbType) -> GbType {
        GbType::String(obj.to_string())
    }

    pub(super) fn export(self) -> GbType {
        GbType::Function(Rc::new(self))
    }
}

impl GbFunc for GbToString {
    fn execute(
        &self,
        _strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
    ) -> GbType {
        if args.len() > 1 {
            panic!("Too many arguments");
        }
        self.to_string(&args[0])
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
        GbType::Function(Rc::new(self))
    }
}

impl GbFunc for GbDump {
    fn execute(
        &self,
        strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
    ) -> GbType {
        if !args.is_empty() {
            panic!("Too many arguments");
        }
        self.dump(strategy)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct GbExit {}

impl GbExit {
    /// Exits the program gracefully
    /// Panics if the provided exit code is not representable as an i32
    pub(super) fn exit(&self, code: GbType) -> GbType {
        match code {
            GbType::Integer(i) => {
                GbType::ReturnValue(Box::new(GbType::ExitSignal(i as i32)))
            }
            _ => GbType::Error(GbError::WrongTypeInFunctionArg),
        }
    }

    pub(super) fn export(self) -> GbType {
        GbType::Function(Rc::new(self))
    }
}

impl GbFunc for GbExit {
    fn execute(
        &self,
        _strategy: &mut dyn InterpreterStrategy,
        args: &[GbType],
    ) -> GbType {
        if args.len() > 1 {
            panic!("Too many arguments");
        } else if args.is_empty() {
            self.exit(GbType::Integer(0))
        } else {
            self.exit(args[0].clone())
        }
    }
}

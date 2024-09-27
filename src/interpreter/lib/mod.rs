use std::rc::Rc;

use super::{gb_type::{GbFunc, GbType}, InterpreterStrategy};

#[derive(Debug, Clone, PartialEq)]
pub(super) struct GbPrint {}

impl GbPrint {
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
    fn execute(&self, _strategy: &mut dyn InterpreterStrategy, args: &[GbType]) -> GbType {
        self.print(args);
        GbType::None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct GbLog {}

impl GbLog {
    pub(super) fn log(&self, info: impl AsRef<str>) {
        tracing::info!("{}", info.as_ref());
    }

    pub(super) fn export(self) -> GbType {
        GbType::Function(Rc::new(self))
    }
}

impl GbFunc for GbLog {
    fn execute(&self, _strategy: &mut dyn InterpreterStrategy, args: &[GbType]) -> GbType {
        if args.len() > 1 {
            panic!("Too many arguments");
        }
        let GbType::String(ref data) = args[0] else {
            return GbType::None;
        };
        self.log(data);
        GbType::None
    }
}

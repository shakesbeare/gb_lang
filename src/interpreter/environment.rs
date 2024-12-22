use crate::interpreter::gb_type::GbType;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Environment {
    symbol_table: HashMap<Rc<str>, GbType>,
}

impl Environment {
    pub fn new(stable: HashMap<Rc<str>, GbType>) -> Self {
        Environment {
            symbol_table: stable,
        }
    }

    pub fn insert<T: Into<Rc<str>>>(&mut self, key: T, value: GbType) -> Rc<str> {
        let key: Rc<str> = key.into();
        self.symbol_table.insert(key.clone(), value);
        key
    }

    pub fn get<T: Into<Rc<str>>>(&self, key: T) -> Option<&GbType> {
        let key: Rc<str> = key.into();
        self.symbol_table.get(&key)
    }

    pub fn get_mut<T: Into<Rc<str>>>(&mut self, key: T) -> Option<&mut GbType> {
        let key: Rc<str> = key.into();
        self.symbol_table.get_mut(&key)
    }

    pub fn inspect(&self) -> Vec<(Rc<str>, &GbType)> {
        let mut out = Vec::new();
        for (k, v) in self.symbol_table.iter() {
            out.push((k.clone(), v));
        }
        out
    }
}

impl Default for Environment {
    fn default() -> Self {
        let mut stable = HashMap::new();
        let mut r#std = HashMap::new();
        r#std.insert("dump".into(), super::lib::GbDump {}.export().into());
        r#std.insert(
            "to_string".into(),
            super::lib::GbToString {}.export().into(),
        );
        r#std.insert("print".into(), super::lib::GbPrint {}.export().into());
        r#std.insert("warn".into(), super::lib::GbWarn {}.export().into());
        r#std.insert("exit".into(), super::lib::GbExit {}.export().into());

        stable.insert("None".into(), GbType::None);
        stable.insert("exit".into(), super::lib::GbExit {}.export());
        stable.insert("std".into(), GbType::Namespace(r#std));
        Self::new(stable)
    }
}

use crate::interpreter::gb_type::GbType;
use gxhash::{HashMap, HashMapExt};
use std::rc::Rc;

pub(super) struct Environment {
    symbol_table: HashMap<Rc<str>, GbType>,
}

impl Environment {
    pub fn new() -> Self {
        let mut stable = HashMap::new();
        stable.insert("None".into(), GbType::None);

        Environment {
            symbol_table: stable,
        }
    }

    pub fn none(&self) -> &GbType {
        self.get("None").expect("None key should never be empty")
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
        Self::new()
    }
}

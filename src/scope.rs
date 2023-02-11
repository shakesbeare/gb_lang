use std::collections::HashMap;

use crate::gb_type::GbType;

pub struct Scope {
    pub identifiers: HashMap<String, GbType>,
}

impl Scope {
    // create a brand new scope with no associations for use when starting the
    // interpreter
    pub fn init() -> Self {
        Scope {
            identifiers: HashMap::new(),
        }
    }

    pub fn lookup(&self, identifier: &String) -> Option<&GbType> {
        if self.identifiers.contains_key(identifier) {
            Some(self.identifiers.get(identifier).unwrap())
        } else {
            None
        }
    }
}

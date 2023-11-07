use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::convert::AsMut;

use crate::gb_type::GbType;
use crate::gb_type::gb_type_of;

use thiserror::Error;

pub struct Scope {
    // Rc<RefCell<T>> allows reference counted, mutable variables at the expensive of being
    // ANNOYING AS HELL. Creating and dereferening these reference chains is part of the cost of
    // building and destroying the stack frames 
    parent: Option<Rc<RefCell<Scope>>>,
    child: Option<Rc<RefCell<Scope>>>,
    identifiers: HashMap<String, usize>,
    values: Vec<Box<GbType>>,
    final_owner: Vec<bool>,
}

impl Scope {
    /// Creates a brand new, clean scope
    pub fn init() -> Self {
        Scope {
            parent: None,
            child: None,
            identifiers: HashMap::new(),
            values: vec![],
            final_owner: vec![],
        }
    }

    /// Creates a new scope as a child of self
    pub fn new_child(&mut self) -> Self {
        let mut new_scope = Scope::init();
        new_scope.parent = Some(Rc::new(RefCell::new(*self)));
        self.child = Some(Rc::new(RefCell::new(new_scope)));
        return new_scope;
    }

    /// Performs clean up tasks to prepare the scope for deallocation
    pub fn clean_up(&mut self) {
        // Move references which need back to parent scope
        let keys = self.identifiers.keys();
        let mut values = vec![];
        for k in keys.clone().into_iter() {
            values.push(self.identifiers.get(k).unwrap());
        }

        let kv = std::iter::zip(keys, values.into_iter());
        let needs_moved: Vec<(&String, &usize)> = kv
            .filter(|(_, x)| {
                if !self.final_owner.get(**x).unwrap() {
                    true
                } else {
                    false
                }
            })
            .collect();

        for (ident, index) in needs_moved.iter() {
            let value = *self.values.swap_remove(**index);
            self.parent.as_mut().unwrap().borrow_mut().bind(ident, value);
        }
    }

    /// Binds the given variables to self by reference
    pub fn using(
        &mut self,
        parent_scope: Scope,
        identifiers: Vec<String>,
    ) -> Result<&mut Scope, ScopeError> {
        for ident in identifiers {
            let value = parent_scope.lookup(&ident);
            let Ok(value) = value else {
                return Err(ScopeError::IdentifierNotBound {
                    ident: ident.to_string(),
                });
            };

            let res = self.bind_ref(&ident, value);

            match res {
                Ok(_) => continue,
                Err(e) => return Err(e),
            };
        }

        Ok(self)
    }

    /// Binds the given variables to self by copy
    pub fn copying(
        &mut self,
        parent_scope: Scope,
        identifiers: Vec<String>,
    ) -> Result<&mut Scope, ScopeError> {
        for ident in identifiers {
            let value = parent_scope.lookup(&ident);
            let Ok(value) = value else {
                return Err(ScopeError::IdentifierNotBound {
                    ident: ident.to_string(),
                });
            };

            let res = self.bind_copy(&ident, &(*value));

            match res {
                Ok(_) => continue,
                Err(e) => return Err(e),
            };
        }

        Ok(self)
    }

    /// Binds the given variables, moving them permanently to self
    pub fn moving(
        &mut self,
        parent_scope: &mut Scope,
        identifiers: Vec<String>,
    ) -> Result<&mut Scope, ScopeError> {
        for ident in identifiers {
            let value = parent_scope.lookup(&ident);
            let Ok(value) = value else {
                return Err(ScopeError::IdentifierNotBound {
                    ident: ident.to_string(),
                });
            };

            let res = self.bind(&ident, *value);

            match res {
                Ok(_) => continue,
                Err(e) => return Err(e),
            };
        }

        Ok(self)
    }

    /// Attempts to bind a new identifier to the given value
    /// This method moves the value to the current scope
    pub fn bind(
        &mut self,
        identifier: &str,
        value: GbType,
    ) -> Result<GbType, ScopeError> {
        if self.identifiers.contains_key(identifier) {
            let index = self.identifiers.get(identifier).unwrap();
            self.values[*index] = Box::new(value);
            Ok(value)
        } else {
            match self.find_available_location() {
                ScopeLocation::Existing(x) => {
                    let obj_p = Box::new(value);
                    self.values[x] = obj_p;
                    self.final_owner[x] = true;
                    Ok(value)
                }
                ScopeLocation::End => {
                    self.values.push(Box::new(value));
                    self.identifiers
                        .insert(identifier.into(), self.values.len() - 1);
                    self.final_owner.push(true);
                    Ok(value)
                }
            }
        }
    }

    /// Attemps to bind a new identifier to the given value
    /// This method expects a Box and flags the value as not belonging to self.
    /// The value will be given back to the parent_scope when self.clean_up is called.
    fn bind_ref(
        &mut self,
        identifier: &str,
        value: Box<GbType>,
    ) -> Result<(), ScopeError> {
        if self.identifiers.contains_key(identifier) {
            let index = self.identifiers.get(identifier).unwrap();
            self.values[*index] = value;
            Ok(())
        } else {
            match self.find_available_location() {
                ScopeLocation::Existing(x) => {
                    self.values[x] = value;
                    self.final_owner[x] = false;
                    Ok(())
                }
                ScopeLocation::End => {
                    self.values.push(value);
                    self.identifiers
                        .insert(identifier.into(), self.values.len() - 1);
                    self.final_owner.push(false);
                    Ok(())
                }
            }
        }
    }

    /// Attempts to bind a new identifier to the given value
    /// This method expects a reference and copies the value behind it to self.
    /// The value will be lost when the scope is cleaned up unless some other process flags it as
    /// not belonging to self
    fn bind_copy(
        &mut self,
        identifier: &str,
        value: &GbType,
    ) -> Result<(), ScopeError> {
        if self.identifiers.contains_key(identifier) {
            let index = self.identifiers.get(identifier).unwrap();
            self.values[*index] = Box::new(value.clone());
            Ok(())
        } else {
            match self.find_available_location() {
                ScopeLocation::Existing(x) => {
                    self.values[x] = Box::new(value.clone());
                    self.final_owner[x] = false;
                    Ok(())
                }
                ScopeLocation::End => {
                    self.values.push(Box::new(value.clone()));
                    self.identifiers
                        .insert(identifier.into(), self.values.len() - 1);
                    self.final_owner.push(false);
                    Ok(())
                }
            }
        }
    }

    /// Attempts to update an existing identifier to a new value
    pub fn update(
        &mut self,
        identifier: &str,
        value: GbType,
    ) -> Result<GbType, ScopeError> {
        let current_value = self.lookup(&identifier.to_string())?;

        let current_type = gb_type_of(*current_value);
        let new_type = gb_type_of(value);

        if current_type != new_type {
            return Err(ScopeError::TypeMismatch { expected: current_type, actual: new_type })
        }
        
        let index = *self.identifiers.get(identifier).unwrap();
        self.values[index] = Box::new(value);

        Ok(value)
    }

    pub fn lookup(
        &self,
        identifier: &String,
    ) -> Result<Box<GbType>, ScopeError> {
        let mut index = 0;
        if self.identifiers.contains_key(identifier) {
            index = *self.identifiers.get(identifier).unwrap();
        } else {
            return Err(ScopeError::IdentifierNotBound {
                ident: identifier.into(),
            });
        };

        return Ok(self.values[index]);
    }

    fn find_available_location(&self) -> ScopeLocation {
        for key in self.identifiers.keys() {
            let value = self.identifiers.get(key).unwrap();
            let data = *self.values[*value];

            match data {
                GbType::Empty => {
                    return ScopeLocation::Existing(value.clone());
                }
                _ => {
                    continue;
                }
            };
        }

        return ScopeLocation::End;
    }
}

#[derive(Debug, Error)]
pub enum ScopeError {
    #[error("Identifier {ident:?} not bound")]
    IdentifierNotBound { ident: String },
    #[error(
        "Cannot redeclare variable {ident:?}: it is not owned by this scope"
    )]
    VariableNotOwned { ident: String },
    #[error("Type mismatch, expected {expected:?} but found {actual:?}")]
    TypeMismatch { expected: String, actual: String },
    #[error("An error occurred while cleaning up the scope")]
    ScopeDeallocationFailed,
}

enum ScopeLocation {
    Existing(usize),
    End,
}

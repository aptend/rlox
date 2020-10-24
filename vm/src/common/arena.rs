use super::lox_string::LoxString;
use super::value::Value;
use std::collections::{HashMap, HashSet};

// TODO: intern identifiers
// pub struct ArenaBuilder {
//     // intern strings
//     strings: HashSet<LoxString>,
//     // intern identifiers
//     identifiers: HashSet<LoxString>,
// }

// impl ArenaBuilder {
//     pub fn new() -> ArenaBuilder {
//         // add global identifiers like `clock`
//     }
//     pub fn build(self) -> Arena {
//         let mut arena = Arena {
//             strings: self.strings,
//             globals: vec![Value::Nil; self.identifiers.len()],
//         };
//     }
// }

#[derive(Default)]
pub struct Arena {
    // intern strings
    strings: HashSet<LoxString>,
    globals: HashMap<LoxString, Value>,
}

impl Arena {
    /// Take the ownership of s, and return a LoxString.  
    /// `takeString` in clox
    pub fn alloc_string(&mut self, s: String) -> LoxString {
        match self.strings.get(s.as_str()) {
            Some(lox_s) => lox_s.clone(),
            None => {
                let lox_s = LoxString::from_owned(s);
                self.strings.insert(lox_s.clone());
                lox_s
            }
        }
    }

    /// Copy the chars from s, and return a LoxString.  
    /// `copyString` in clox
    pub fn alloc_string_ref(&mut self, s: &str) -> LoxString {
        match self.strings.get(s) {
            Some(lox_s) => lox_s.clone(),
            None => {
                let lox_s = LoxString::from_ref(s);
                self.strings.insert(lox_s.clone());
                lox_s
            }
        }
    }

    pub fn define_global(&mut self, key: LoxString, val: Value) {
        self.globals.insert(key, val);
    }

    pub fn get_global(&mut self, key: &LoxString) -> Option<Value> {
        self.globals.get(key).cloned()
    }
}

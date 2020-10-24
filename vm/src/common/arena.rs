use super::lox_string::LoxString;
use std::collections::HashSet;

#[derive(Default)]
pub struct Arena {
    // intern strings
    strings: HashSet<LoxString>,
}

impl Arena {
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
}

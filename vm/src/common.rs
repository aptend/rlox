//! structs bridges between compiling and running

use std::fmt;
use std::rc::Rc;

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

// LoxString is immutable. It should be cheap to clone into stack.
// Rc<String> has 8 bytes, which keep the size of Value 16 bytes
// Compare:
//   String has 24 bytes: ptr + cap + len
//   Rc<str> has 16 bytes, it's a fat pointer, ptr + len
#[derive(Debug, Clone)]
pub struct LoxString(Rc<String>);

impl LoxString {
    // !! these two from converters must be called after global intern. !!
    // could have impl them using converter::From, but i think it is better
    // to make them more explicit and less visible
    pub(crate) fn from_owned(s: String) -> Self {
        LoxString(Rc::new(s))
    }
    // TODO: AsRef
    pub(crate) fn from_ref(s: &str) -> Self {
        LoxString(Rc::new(s.to_string()))
    }
}

impl std::cmp::PartialEq for LoxString {
    // intern strings makes it correct to do ptr comparing
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl std::hash::Hash for LoxString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl std::cmp::Eq for LoxString {}

// Hash matches between LoxString and str, but Eq doesn't
// This is Ok because we have interned **all** LoxStrings
// There is no way to have 2 different LoxStrings with the same literal str.
impl std::borrow::Borrow<str> for LoxString {
    fn borrow(&self) -> &str {
        &self.0
    }
}
use std::ops::Deref;
impl Deref for LoxString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    String(LoxString),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{:?}", s.deref()),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Boolean(false) => false,
            _ => true,
        }
    }
}

#[derive(Clone, Debug, Copy, Eq, PartialEq, Default)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Position {
        Position { line, column }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}, column {}]", self.line, self.column)
    }
}

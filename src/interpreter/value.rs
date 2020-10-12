use super::{Callable, LoxCallable, LoxInstance};
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    String(RcString),
    Number(f64),
    Boolean(bool),
    Callable(Callable),
    Instance(LoxInstance),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Boolean(false) => false,
            _ => true,
        }
    }

    pub fn new_callable(callee: Box<dyn LoxCallable>) -> Value {
        Value::Callable(Callable::new(callee))
    }
    pub fn new_string(s: &str) -> Value {
        Value::String(RcString::new(s))
    }
}

impl std::default::Default for Value {
    fn default() -> Self {
        Value::Nil
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::String(s) => write!(f, "{}", s.deref()),
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Callable(c) => write!(f, "{:?}", c.deref().deref()),
            Value::Instance(i) => write!(f, "{:?}", i),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RcString {
    inner: Rc<String>,
}

// make Value::String cloning cheap
impl RcString {
    pub fn new(s: &str) -> RcString {
        RcString {
            inner: Rc::new(s.to_owned()),
        }
    }
}

impl Deref for RcString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}

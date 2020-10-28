use super::{LoxFunction, LoxString, NativeFunction};

use std::fmt;

#[derive(Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    String(LoxString),
    Function(LoxFunction),
    NativeFn(NativeFunction)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Function(fun) => write!(f, "{}", fun),
            Value::NativeFn(fun) => write!(f, "{}", fun),
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

use std::fmt;

pub enum Value {
    Number(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
        }
    }
}

use crate::chunk::Constant;

impl std::convert::From<&Constant> for Value {
    fn from(val: &Constant) -> Self {
        match val {
            Constant::Number(v) => Value::Number(*v),
        }
    }
}

use std::fmt;
use std::rc::Rc;

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

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    // Rc<str> will reduce size of Value, 32 bytes -> 16 bytes
    // String has 24 bytes: ptr + cap + len
    // and it is cheap to clone Value::String
    String(Rc<str>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{:?}", s),
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

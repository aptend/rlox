use std::fmt;

use crate::common::Position;
pub struct RuntimeError {
    msg: String,
    pos: Position,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} RuntimeError: {}", self.pos, self.msg)
    }
}

impl RuntimeError {
    pub fn new(pos: Position, msg: &str) -> Self {
        RuntimeError {
            msg: msg.to_string(),
            pos
        }
    }
}


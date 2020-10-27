//! Structs bridging comipiling and executing

mod arena;
mod lox_string;
mod value;
mod chunk;

use std::fmt;

pub use arena::Arena;
pub use lox_string::LoxString;
pub use value::Value;
pub use chunk::{Chunk, Instruction};

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

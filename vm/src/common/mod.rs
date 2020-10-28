//! Structs bridging comipiling and executing

mod arena;
mod chunk;
mod lox_function;
mod lox_string;
mod native_fn;
mod value;

use std::fmt;

pub use arena::Arena;
pub use chunk::{Chunk, Instruction};
pub use lox_function::{LoxFunInner, LoxFunction};
pub use lox_string::LoxString;
pub use native_fn::{NATIVECLOCK, NativeFunction};
pub use value::Value;

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

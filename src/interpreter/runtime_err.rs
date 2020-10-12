use super::{Token, TokenKind, Value};
use std::fmt;

type BoxToken = Box<Token>;
pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Debug)]
pub enum RuntimeError {
    UnaryMismatchedType(BoxToken),
    BinaryMismatchedType(BoxToken),
    UndefinedIdentifier(BoxToken),
    NonCallable(BoxToken),
    // maxium argmument counts is 255, u8 is enough.
    ArityMismatch(BoxToken, u8, u8),

    // Not visible for user, interpreter use it to break loop
    BreakControl,
    ReturnControl(Value),
}

fn write_position(f: &mut fmt::Formatter<'_>, token: &Token) -> fmt::Result {
    if token.kind == TokenKind::EOF {
        write!(f, "[the last line] RuntimeError: ")
    } else {
        let (line, col) = (token.position.line, token.position.column);
        write!(f, "[line {}, column {}] RuntimeError: ", line, col)
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::UnaryMismatchedType(token) => {
                write_position(f, token)?;
                write!(f, "Operand of unary op '-' must be a number.")
            }
            RuntimeError::BinaryMismatchedType(token) => {
                write_position(f, token)?;
                write!(f, "Operands must be numbers")
            }
            RuntimeError::UndefinedIdentifier(token) => {
                write_position(f, token)?;
                write!(f, "Undefined identifier: {}", token.as_str().unwrap())
            }
            RuntimeError::NonCallable(token) => {
                write_position(f, token)?;
                write!(f, "Can only call functions and classes")
            }
            RuntimeError::ArityMismatch(token, expect, got) => {
                write_position(f, token)?;
                write!(f, "Expected {} arguments but got {}", expect, got)
            }
            RuntimeError::BreakControl => write!(f, "break control signal"),
            RuntimeError::ReturnControl(_) => {
                write!(f, "return control signal")
            }
        }
    }
}

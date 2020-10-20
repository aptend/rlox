use std::fmt;

use crate::scanner::{ScanError, Token};
pub struct CompileError {
    msg: String,
    token: Token,
}

pub enum SyntaxError {
    CompileError(CompileError),
    ScanError(ScanError),
}

impl SyntaxError {
    pub fn new_compiler_err(token: Token, msg: &str) -> SyntaxError {
        SyntaxError::CompileError(CompileError {
            msg: msg.to_owned(),
            token,
        })
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxError::ScanError(e) => write!(f, "{}", e),
            SyntaxError::CompileError(e) => write!(f, "{}", e),
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} SyntaxError at '{}': {}",
            self.token.position,
            self.token.as_str(),
            self.msg
        )
    }
}

impl std::convert::From<ScanError> for SyntaxError {
    fn from(err: ScanError) -> Self {
        SyntaxError::ScanError(err)
    }
}

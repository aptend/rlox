use std::fmt;

use super::token::Position;

enum ErrorKind {
    UnexpectedChar(char),
    UnterminatedString,
}

pub struct ScanError {
    kind: ErrorKind,
    pos: Position,
}

impl ScanError {
    pub fn new_unexpected_char(ch: char, line: usize, column: usize) -> Self {
        ScanError {
            kind: ErrorKind::UnexpectedChar(ch),
            pos: Position { line, column },
        }
    }

    pub fn new_unterminated_str(line: usize, column: usize) -> Self {
        ScanError {
            kind: ErrorKind::UnterminatedString,
            pos: Position { line, column },
        }
    }
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        let msg = match self.kind {
            ErrorKind::UnterminatedString => "unterminated string",
            ErrorKind::UnexpectedChar(ch) => {
                s.push_str("unexpected character '");
                s.push(ch);
                s.push('\'');
                s.as_str()
            }
        };
        write!(f, "{} SyntaxError: {}", self.pos, msg)
    }
}

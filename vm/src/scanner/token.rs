use std::fmt;
use std::str;

use crate::common::Position;

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum TokenKind {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    SPACE,
    COMMENT,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER(String),
    STRING(String),
    NUMBER(f64),

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    BREAK,
}

impl std::cmp::PartialEq for TokenKind {
    fn eq(&self, other: &Self) -> bool {
        // we only compare the type of variant, let its value untouched.
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

#[derive(Clone)]
pub struct Token {
    pub position: Position,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(line: usize, column: usize, kind: TokenKind) -> Token {
        Token {
            position: Position::new(line, column),
            kind,
        }
    }

    pub(crate) fn with_kind(kind: TokenKind) -> Token {
        Token {
            position: Position::default(),
            kind,
        }
    }

    pub(crate) fn new_this() -> Token {
        Token::with_kind(TokenKind::THIS)
    }

    pub(crate) fn new_super() -> Token {
        Token::with_kind(TokenKind::SUPER)
    }

    pub fn as_str(&self) -> &str {
        match &self.kind {
            TokenKind::LEFT_PAREN => "(",
            TokenKind::RIGHT_PAREN => ")",
            TokenKind::LEFT_BRACE => "{",
            TokenKind::RIGHT_BRACE => "}",
            TokenKind::COMMA => ",",
            TokenKind::DOT => ".",
            TokenKind::MINUS => "-",
            TokenKind::PLUS => "+",
            TokenKind::SEMICOLON => ";",
            TokenKind::SLASH => "/",
            TokenKind::STAR => "*",
            TokenKind::SPACE => " ",
            TokenKind::COMMENT => "/**/",
            TokenKind::BANG => "!",
            TokenKind::BANG_EQUAL => "!=",
            TokenKind::EQUAL => "=",
            TokenKind::EQUAL_EQUAL => "==",
            TokenKind::GREATER => ">",
            TokenKind::GREATER_EQUAL => ">=",
            TokenKind::LESS => "<",
            TokenKind::LESS_EQUAL => "<=",
            TokenKind::IDENTIFIER(s) => s,
            TokenKind::STRING(s) => s,
            TokenKind::NUMBER(_) => "f64",
            TokenKind::AND => "and",
            TokenKind::CLASS => "class",
            TokenKind::ELSE => "else",
            TokenKind::FALSE => "false",
            TokenKind::FUN => "fun",
            TokenKind::FOR => "for",
            TokenKind::IF => "if",
            TokenKind::NIL => "nil",
            TokenKind::OR => "or",
            TokenKind::PRINT => "print",
            TokenKind::RETURN => "return",
            TokenKind::SUPER => "super",
            TokenKind::THIS => "this",
            TokenKind::TRUE => "true",
            TokenKind::VAR => "var",
            TokenKind::WHILE => "while",
            TokenKind::BREAK => "break",
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TokenKind::NUMBER(n) => write!(f, "{}", n),
            _ => write!(f, "{}", self.as_str()),
        }
    }
}

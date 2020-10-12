use std::fmt;
use std::str;

#[allow(non_camel_case_types)]
#[derive(Clone, Debug)]
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

    EOF,
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
    pub fn new(position: Position, kind: TokenKind) -> Token {
        Token { position, kind }
    }

    pub(crate) fn with_kind(kind: TokenKind) -> Token {
        Token {
            position: Position::default(),
            kind,
        }
    }

    pub fn as_str(&self) -> Option<&String> {
        match &self.kind {
            TokenKind::IDENTIFIER(s) => Some(s),
            TokenKind::STRING(s) => Some(s),
            _ => None,
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TokenKind::PLUS => write!(f, "+"),
            TokenKind::MINUS => write!(f, "-"),
            TokenKind::SLASH => write!(f, "/"),
            TokenKind::STAR => write!(f, "*"),
            TokenKind::LESS => write!(f, "<"),
            TokenKind::LESS_EQUAL => write!(f, "<="),
            TokenKind::EQUAL => write!(f, "="),
            TokenKind::EQUAL_EQUAL => write!(f, "=="),
            TokenKind::BANG_EQUAL => write!(f, "!="),
            TokenKind::GREATER => write!(f, ">"),
            TokenKind::GREATER_EQUAL => write!(f, ">="),
            TokenKind::SEMICOLON => write!(f, ";"),
            other => write!(f, "{:?}", other),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Default)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Position {
        Position { line, column }
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}, column {}]", self.line, self.column)
    }
}

enum ErrorKind {
    UnexpectedChar(char),
    UnterminatedString,
}

pub struct ScanError {
    kind: ErrorKind,
    pos: Position,
}

impl fmt::Debug for ScanError {
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
        write!(f, "{:?} SyntaxError: {}", self.pos, msg)
    }
}

pub struct Scanner<'a> {
    // position
    line: usize,
    column: usize,

    // working buffer
    current_lexeme: String,
    // look ahread two chars, manually
    peek1: Option<char>,
    peek2: Option<char>,

    // iterator of chars of source code
    source: str::Chars<'a>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            line: 1,
            column: 1,
            current_lexeme: String::new(),
            peek1: None,
            peek2: None,
            source: source.chars(),
        }
    }

    /// peek and check the 1st char in the code stream
    fn peek1_check<F: Fn(char) -> bool>(&mut self, check: F) -> bool {
        let p1 = if self.peek1.is_some() {
            self.peek1
        } else if self.peek2.is_some() {
            self.peek1 = self.peek2.take();
            self.peek1
        } else {
            self.peek1 = self.source.next();
            self.peek1
        };

        match p1 {
            Some(c) => check(c),
            None => false,
        }
    }

    /// peek the 2nd char in the code stream
    /// ensure peek1 was called before calling peek2
    fn peek2_check<F: Fn(char) -> bool>(&mut self, check: F) -> bool {
        assert!(self.peek1.is_some());
        let p2 = if self.peek2.is_some() {
            self.peek2
        } else {
            self.peek2 = self.source.next();
            self.peek2
        };

        match p2 {
            Some(c) => check(c),
            None => false,
        }
    }

    fn advance(&mut self) -> Option<char> {
        // let mut next: char;
        let next = if self.peek1.is_some() {
            self.peek1.take()
        } else if self.peek2.is_some() {
            self.peek2.take()
        } else {
            self.source.next()
        };
        if let Some(ch) = next {
            self.current_lexeme.push(ch);
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        next
    }

    fn advance_if_eq(&mut self, ch: char) -> bool {
        if self.peek1_check(|c| ch == c) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance_while<F: Fn(char) -> bool>(&mut self, cond: F) {
        while self.peek1_check(&cond) {
            self.advance();
        }
    }

    fn collect_string(&mut self) -> Result<TokenKind, ScanError> {
        self.advance_while(|c| c != '"');
        // stop because of eof
        if self.peek1.is_none() {
            return Err(ScanError {
                pos: Position::new(self.line, self.column),
                kind: ErrorKind::UnterminatedString,
            });
        }
        // consume the trailing "
        self.advance();
        let n = self.current_lexeme.len();
        let content = &self.current_lexeme[1..n - 1];
        Ok(TokenKind::STRING(content.to_string()))
    }

    fn collect_number(&mut self) -> Result<TokenKind, ScanError> {
        self.advance_while(&is_digit);
        if self.peek1_check(|c| c == '.') && self.peek2_check(&is_digit) {
            // consume '.'
            self.advance();
            self.advance_while(&is_digit);
        }
        let n: f64 = self.current_lexeme.parse().unwrap();
        Ok(TokenKind::NUMBER(n))
    }

    fn collect_identifier(&mut self) -> Result<TokenKind, ScanError> {
        self.advance_while(&is_alphanumeric);
        match self.current_lexeme.as_str() {
            "and" => Ok(TokenKind::AND),
            "class" => Ok(TokenKind::CLASS),
            "else" => Ok(TokenKind::ELSE),
            "false" => Ok(TokenKind::FALSE),
            "fun" => Ok(TokenKind::FUN),
            "for" => Ok(TokenKind::FOR),
            "if" => Ok(TokenKind::IF),
            "nil" => Ok(TokenKind::NIL),
            "or" => Ok(TokenKind::OR),
            "print" => Ok(TokenKind::PRINT),
            "return" => Ok(TokenKind::RETURN),
            "super" => Ok(TokenKind::SUPER),
            "this" => Ok(TokenKind::THIS),
            "true" => Ok(TokenKind::TRUE),
            "var" => Ok(TokenKind::VAR),
            "while" => Ok(TokenKind::WHILE),
            "break" => Ok(TokenKind::BREAK),
            other => Ok(TokenKind::IDENTIFIER(other.to_owned())),
        }
    }
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_alpha(c: char) -> bool {
    match c {
        _x @ 'a'..='z' => true,
        _x @ 'A'..='Z' => true,
        '_' => true,
        _ => false,
    }
}

fn is_alphanumeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}

fn is_whitespace(c: char) -> bool {
    match c {
        ' ' | '\r' | '\t' | '\n' => true,
        _ => false,
    }
}

impl<'a> std::iter::Iterator for Scanner<'a> {
    type Item = Result<Token, ScanError>;

    fn next(&mut self) -> Option<Self::Item> {
        let current_pos = Position::new(self.line, self.column);
        self.current_lexeme.clear();
        if let Some(ch) = self.advance() {
            let kind = match ch {
                // single char
                '(' => Ok(TokenKind::LEFT_PAREN),
                ')' => Ok(TokenKind::RIGHT_PAREN),
                '{' => Ok(TokenKind::LEFT_BRACE),
                '}' => Ok(TokenKind::RIGHT_BRACE),
                ',' => Ok(TokenKind::COMMA),
                '.' => Ok(TokenKind::DOT),
                '-' => Ok(TokenKind::MINUS),
                '+' => Ok(TokenKind::PLUS),
                ';' => Ok(TokenKind::SEMICOLON),
                '*' => Ok(TokenKind::STAR),
                '/' => {
                    if self.advance_if_eq('/') {
                        self.advance_while(|c| c != '\n');
                        Ok(TokenKind::COMMENT)
                    } else {
                        Ok(TokenKind::SLASH)
                    }
                }
                c if is_whitespace(c) => Ok(TokenKind::SPACE),
                // cmp
                '!' => Ok(if self.advance_if_eq('=') {
                    TokenKind::BANG_EQUAL
                } else {
                    TokenKind::BANG
                }),
                '<' => Ok(if self.advance_if_eq('=') {
                    TokenKind::LESS_EQUAL
                } else {
                    TokenKind::LESS
                }),
                '>' => Ok(if self.advance_if_eq('=') {
                    TokenKind::GREATER_EQUAL
                } else {
                    TokenKind::GREATER
                }),
                '=' => Ok(if self.advance_if_eq('=') {
                    TokenKind::EQUAL_EQUAL
                } else {
                    TokenKind::EQUAL
                }),
                '"' => self.collect_string(),
                c if is_digit(c) => self.collect_number(),
                c if is_alpha(c) => self.collect_identifier(),
                _ => Err(ScanError {
                    pos: current_pos,
                    kind: ErrorKind::UnexpectedChar(ch),
                }),
            };

            match kind {
                // ignore whitespace and comments
                Ok(TokenKind::SPACE) | Ok(TokenKind::COMMENT) => self.next(),
                // find useful one
                Ok(kind) => Some(Ok(Token::new(current_pos, kind))),
                Err(e) => Some(Err(e)),
            }
        } else {
            None
        }
    }
}

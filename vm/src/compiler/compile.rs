use super::error::SyntaxError;
use crate::chunk::*;
use crate::common::{Position, Value};
use crate::scanner::*;

use super::prec::Precedence;

use lazy_static::lazy_static;

type CompileResult<T> = Result<T, SyntaxError>;

lazy_static! {
    static ref LEFT_PAREN: TokenKind = TokenKind::LEFT_PAREN;
    static ref RIGHT_PAREN: TokenKind = TokenKind::RIGHT_PAREN;
    static ref LEFT_BRACE: TokenKind = TokenKind::LEFT_BRACE;
    static ref RIGHT_BRACE: TokenKind = TokenKind::RIGHT_BRACE;
    static ref COMMA: TokenKind = TokenKind::COMMA;
    static ref DOT: TokenKind = TokenKind::DOT;
    static ref MINUS: TokenKind = TokenKind::MINUS;
    static ref PLUS: TokenKind = TokenKind::PLUS;
    static ref SEMICOLON: TokenKind = TokenKind::SEMICOLON;
    static ref SLASH: TokenKind = TokenKind::SLASH;
    static ref STAR: TokenKind = TokenKind::STAR;
    static ref SPACE: TokenKind = TokenKind::SPACE;
    static ref COMMENT: TokenKind = TokenKind::COMMENT;
    static ref BANG: TokenKind = TokenKind::BANG;
    static ref BANG_EQUAL: TokenKind = TokenKind::BANG_EQUAL;
    static ref EQUAL: TokenKind = TokenKind::EQUAL;
    static ref EQUAL_EQUAL: TokenKind = TokenKind::EQUAL_EQUAL;
    static ref GREATER: TokenKind = TokenKind::GREATER;
    static ref GREATER_EQUAL: TokenKind = TokenKind::GREATER_EQUAL;
    static ref LESS: TokenKind = TokenKind::LESS;
    static ref LESS_EQUAL: TokenKind = TokenKind::LESS_EQUAL;
    static ref IDENTIFIER: TokenKind = TokenKind::IDENTIFIER(String::new());
    static ref STRING: TokenKind = TokenKind::STRING(String::new());
    static ref NUMBER: TokenKind = TokenKind::NUMBER(0.0);
    static ref AND: TokenKind = TokenKind::AND;
    static ref CLASS: TokenKind = TokenKind::CLASS;
    static ref ELSE: TokenKind = TokenKind::ELSE;
    static ref FALSE: TokenKind = TokenKind::FALSE;
    static ref FUN: TokenKind = TokenKind::FUN;
    static ref FOR: TokenKind = TokenKind::FOR;
    static ref IF: TokenKind = TokenKind::IF;
    static ref NIL: TokenKind = TokenKind::NIL;
    static ref OR: TokenKind = TokenKind::OR;
    static ref PRINT: TokenKind = TokenKind::PRINT;
    static ref RETURN: TokenKind = TokenKind::RETURN;
    static ref SUPER: TokenKind = TokenKind::SUPER;
    static ref THIS: TokenKind = TokenKind::THIS;
    static ref TRUE: TokenKind = TokenKind::TRUE;
    static ref VAR: TokenKind = TokenKind::VAR;
    static ref WHILE: TokenKind = TokenKind::WHILE;
    static ref BREAK: TokenKind = TokenKind::BREAK;
}

pub struct Compiler<'a> {
    errors: Vec<SyntaxError>,
    peeked: Option<Token>,
    tokens: Scanner<'a>,
    chunk: &'a mut Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(scanner: Scanner<'a>, chunk: &'a mut Chunk) -> Compiler<'a> {
        Compiler {
            errors: Vec::new(),
            peeked: None,
            tokens: scanner,
            chunk,
        }
    }

    fn advance(&mut self) -> Option<Token> {
        if self.peeked.is_some() {
            self.peeked.take()
        } else {
            // loop to find one valid token
            loop {
                if let Some(res) = self.tokens.next() {
                    match res {
                        Ok(token) => return Some(token),
                        Err(e) => self.errors.push(e.into()),
                    }
                } else {
                    return None;
                }
            }
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = self.advance();
        }
        self.peeked.as_ref()
    }

    fn peek_check<F: FnOnce(&TokenKind) -> bool>(&mut self, f: F) -> bool {
        if let Some(tk) = self.peek() {
            if f(&tk.kind) {
                return true;
            }
        }
        false
    }

    fn advance_if_eq(&mut self, kind: &TokenKind) -> Option<Token> {
        if self.peek_check(|k| k == kind) {
            return self.advance();
        }
        None
    }

    fn consume_or_err(
        &mut self,
        kind: &TokenKind,
        msg: &str,
    ) -> CompileResult<Token> {
        if let Some(tk) = self.advance_if_eq(kind) {
            Ok(tk)
        } else {
            Err(SyntaxError::new_compiler_err(self.peek().cloned(), msg))
        }
    }

    fn emit_return(&mut self) {
        self.chunk
            .push_instr(Instruction::Return, Position::default());
    }

    fn emit_instr(&mut self, instr: Instruction, pos: Position) {
        self.chunk.push_instr(instr, pos);
    }

    fn expression(&mut self) {
        self.parse_with(Precedence::Assign);
    }

    fn grouping(&mut self) {
        self.advance(); // skip '('
        self.expression();
        self.consume_or_err(&RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn constant(&mut self) {
        let tk = self.advance().unwrap();
        let value = match &tk.kind {
            TokenKind::NUMBER(f) => Value::Number(*f),
            _ => unimplemented!(),
        };
        self.emit_instr(Instruction::LoadConstant(value), tk.position);
    }

    fn unary(&mut self) {
        let tk = self.advance().unwrap();
        self.expression();
        let instr = match &tk.kind {
            TokenKind::MINUS => Instruction::Negate,
            _ => unreachable!(),
        };
        self.emit_instr(instr, tk.position);
    }

    fn parse_with(&mut self, prec: Precedence) {
        self.dispatch_prefix();
    }

    // use match expression to mock a the prefix table
    fn dispatch_prefix(&mut self) {
        match self.peek().and_then(|tk| Some(&tk.kind)) {
            Some(&TokenKind::NUMBER(_)) => self.constant(),
            Some(&TokenKind::LEFT_PAREN) => self.grouping(),
            Some(&TokenKind::MINUS) => self.unary(),
            _ => {
                SyntaxError::new_compiler_err(
                    self.advance(),
                    "Expect an expression.",
                );
            }
        }
    }


    pub fn compile(&mut self) {
        self.emit_return();
    }
}

use super::error::SyntaxError;
use crate::chunk::*;
use crate::common::{Arena, Position, Value};
use crate::scanner::*;

use super::prec::Precedence;

use lazy_static::lazy_static;

use std::mem;

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
    static ref QUESTION: TokenKind = TokenKind::QUESTION;
    static ref COLON: TokenKind = TokenKind::COLON;
}

pub struct Compiler<'a> {
    errors: Vec<SyntaxError>,
    peeked: Option<Token>,
    tokens: Scanner<'a>,
    arena: Arena,
    chunk: Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(scanner: Scanner<'a>, program_name: &str) -> Compiler<'a> {
        Compiler {
            errors: Vec::new(),
            peeked: None,
            tokens: scanner,
            arena: Arena::default(),
            chunk: Chunk::new(program_name),
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

    fn peek(&mut self) -> Option<&TokenKind> {
        if self.peeked.is_none() {
            self.peeked = self.advance();
        }
        self.peeked.as_ref().map(|t| &t.kind)
    }

    fn peek_check<F: FnOnce(&TokenKind) -> bool>(&mut self, f: F) -> bool {
        match self.peek() {
            Some(tk) => f(tk),
            None => false,
        }
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
            Err(SyntaxError::new_compiler_err(self.peeked.clone(), msg))
        }
    }

    fn emit_return(&mut self, pos: Option<Position>) {
        let pos = match pos {
            Some(p) => p,
            None => Position::default(),
        };
        self.chunk.push_instr(Instruction::Return, pos);
    }

    fn emit_instr(
        &mut self,
        instr: Instruction,
        pos: Position,
    ) -> CompileResult<()> {
        self.chunk.push_instr(instr, pos);
        Ok(())
    }

    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    // ----------------   expressions parsing   -----------------------------
    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------

    fn expression(&mut self) -> CompileResult<()> {
        self.parse_with(Precedence::Assign)
    }

    fn variable(&mut self, can_assign: bool) -> CompileResult<()> {
        let tk = self.advance().unwrap();
        let identifier = self.arena.alloc_string_ref(tk.as_str());
        // Assignment expression has no handler in `dispatch_prefix`,
        // so we have to tackle with its precedence manually.
        // variable has two references:
        //   1. dispatch_prefix
        //   2. call TODO
        if can_assign && self.advance_if_eq(&EQUAL).is_some() {
            self.expression()?; // more assignment
            self.emit_instr(Instruction::SetGlobal(identifier), tk.position)
        } else {
            self.emit_instr(Instruction::GetGlobal(identifier), tk.position)
        }
    }

    fn grouping(&mut self) -> CompileResult<()> {
        self.advance(); // skip '('
        self.expression()?;
        self.consume_or_err(&RIGHT_PAREN, "Expect ')' after expression.")
            .and(Ok(()))
    }

    fn constant(&mut self) -> CompileResult<()> {
        let tk = self.advance().unwrap();
        let value = match &tk.kind {
            TokenKind::NUMBER(f) => Value::Number(*f),
            // FIXME: copy the string from source code, waste some memory
            TokenKind::STRING(s) => {
                Value::String(self.arena.alloc_string_ref(s.as_str()))
            }
            _ => unimplemented!(),
        };
        self.emit_instr(Instruction::LoadConstant(value), tk.position)
    }

    fn literal(&mut self) -> CompileResult<()> {
        let tk = self.advance().unwrap();
        let instr = match &tk.kind {
            TokenKind::NIL => Instruction::Nil,
            TokenKind::TRUE => Instruction::True,
            TokenKind::FALSE => Instruction::False,
            _ => unimplemented!(),
        };
        self.emit_instr(instr, tk.position)
    }

    fn unary(&mut self) -> CompileResult<()> {
        let tk = self.advance().unwrap();
        // compose those expressions with higher or equal level precedence,
        // which means it is right-associated for unary comp
        self.parse_with(Precedence::Unary)?;
        let instr = match &tk.kind {
            TokenKind::MINUS => Instruction::Negate,
            TokenKind::BANG => Instruction::Not,
            _ => unreachable!(),
        };
        self.emit_instr(instr, tk.position)
    }

    fn binary(&mut self) -> CompileResult<()> {
        let tk = self.advance().unwrap();
        // compose those expressions with higher level precedence
        // which is left-associated
        self.parse_with(Precedence::of(Some(&tk.kind)).next_prec())?;
        let instr = match &tk.kind {
            TokenKind::PLUS => Instruction::Add,
            TokenKind::MINUS => Instruction::Subtract,
            TokenKind::STAR => Instruction::Multiply,
            TokenKind::SLASH => Instruction::Divide,
            TokenKind::EQUAL_EQUAL => Instruction::Equal,
            TokenKind::BANG_EQUAL => Instruction::Equal,
            TokenKind::LESS => Instruction::Less,
            TokenKind::LESS_EQUAL => Instruction::Greater,
            TokenKind::GREATER => Instruction::Greater,
            TokenKind::GREATER_EQUAL => Instruction::Less,
            _ => unreachable!(),
        };
        self.emit_instr(instr, tk.position)?;
        if &tk.kind == &*LESS_EQUAL
            || &tk.kind == &*GREATER_EQUAL
            || &tk.kind == &*BANG_EQUAL
        {
            self.emit_instr(Instruction::Not, tk.position)?;
        }
        Ok(())
    }

    fn ternary(&mut self) -> CompileResult<()> {
        let tk = self.advance().unwrap();
        // right-associated:
        // 1>0?10:20?100:101 will expand to 1>0? 10 : (20?100:101) = 10
        // and we don't want this: cond ? A=foo : B=bar
        self.parse_with(Precedence::Ternary)?;
        self.consume_or_err(&COLON, "Expect ':' in ternary expression")?;
        self.parse_with(Precedence::Ternary)?;
        self.emit_instr(Instruction::Ternary, tk.position)
    }

    fn parse_with(&mut self, prec: Precedence) -> CompileResult<()> {
        let can_assign = prec <= Precedence::Assign;
        self.dispatch_prefix(can_assign)?;
        // Precedence::of is a 'flatterer' of Rust borrow checker.
        // The reference returned by self.peek() holds &mut self, it
        // can't be passed into a method like self.precedence_of(kind)
        while prec <= Precedence::of(self.peek()) {
            self.dispatch_infix()?;
        }
        if can_assign && self.peek_check(|k| k == &*EQUAL) {
            return Err(SyntaxError::new_compiler_err(
                self.advance(),
                "Invalid assignment target.",
            ));
        }
        Ok(())
    }

    // use match expression to mock a the prefix table
    fn dispatch_prefix(&mut self, can_assign: bool) -> CompileResult<()> {
        match self.peek() {
            Some(&TokenKind::NUMBER(_)) => self.constant(),
            Some(&TokenKind::STRING(_)) => self.constant(),
            Some(&TokenKind::IDENTIFIER(_)) => self.variable(can_assign),
            Some(&TokenKind::LEFT_PAREN) => self.grouping(),
            Some(&TokenKind::MINUS) => self.unary(),
            Some(&TokenKind::BANG) => self.unary(),
            Some(&TokenKind::NIL) => self.literal(),
            Some(&TokenKind::TRUE) => self.literal(),
            Some(&TokenKind::FALSE) => self.literal(),
            _ => Err(SyntaxError::new_compiler_err(
                self.advance(),
                "Expect an expression.",
            )),
        }
    }

    fn dispatch_infix(&mut self) -> CompileResult<()> {
        match self.peek() {
            Some(&TokenKind::PLUS)
            | Some(&TokenKind::MINUS)
            | Some(&TokenKind::STAR)
            | Some(&TokenKind::SLASH)
            | Some(&TokenKind::LESS)
            | Some(&TokenKind::LESS_EQUAL)
            | Some(&TokenKind::EQUAL_EQUAL)
            | Some(&TokenKind::BANG_EQUAL)
            | Some(&TokenKind::GREATER)
            | Some(&TokenKind::GREATER_EQUAL) => self.binary(),
            Some(&TokenKind::QUESTION) => self.ternary(),
            _ => unreachable!(),
        }
    }

    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    // -----------------   statements parsing   -----------------------------
    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------

    fn var_decl(&mut self) -> CompileResult<()> {
        let tk = self.consume_or_err(&IDENTIFIER, "Expect varibale name")?;
        let identifier = self.arena.alloc_string_ref(tk.as_str());
        if self.advance_if_eq(&EQUAL).is_some() {
            self.expression()?;
        } else {
            self.emit_instr(Instruction::Nil, tk.position)?;
        }
        self.consume_or_err(
            &SEMICOLON,
            "Expect ';' after variable declaration.",
        )?;
        self.emit_instr(Instruction::DefGlobal(identifier), tk.position)
    }

    fn print_stmt(&mut self) -> CompileResult<()> {
        self.expression()?;
        let tk = self.consume_or_err(&SEMICOLON, "Expect ';' after value.")?;
        self.emit_instr(Instruction::Print, tk.position)
    }

    fn expression_stmt(&mut self) -> CompileResult<()> {
        self.expression()?;
        let tk =
            self.consume_or_err(&SEMICOLON, "Expect ';' after expression.")?;
        self.emit_instr(Instruction::Pop, tk.position)
    }

    fn statement(&mut self) -> CompileResult<()> {
        if self.advance_if_eq(&PRINT).is_some() {
            return self.print_stmt();
        } else {
            self.expression_stmt()
        }
    }

    fn declaration(&mut self) -> CompileResult<()> {
        if self.advance_if_eq(&VAR).is_some() {
            self.var_decl()
        } else {
            self.statement()
        }
    }

    fn synchronize(&mut self) {
        while let Some(kind) = self.peek() {
            match kind {
                TokenKind::SEMICOLON => {
                    self.advance();
                    return;
                }
                TokenKind::CLASS
                | TokenKind::FUN
                | TokenKind::VAR
                | TokenKind::FOR
                | TokenKind::IF
                | TokenKind::WHILE
                | TokenKind::PRINT
                | TokenKind::RETURN => return,
                _ => {
                    self.advance();
                }
            }
        }
    }

    pub fn compile(&mut self) -> Result<(Chunk, Arena), Vec<SyntaxError>> {
        while self.peek().is_some() {
            if let Err(e) = self.declaration() {
                self.errors.push(e);
                self.synchronize();
            }
        }
        if self.errors.is_empty() {
            self.emit_return(None);
            Ok((mem::take(&mut self.chunk), mem::take(&mut self.arena)))
        } else {
            Err(mem::take(&mut self.errors))
        }
    }
}

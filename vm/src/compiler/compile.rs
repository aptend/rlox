use super::error::SyntaxError;
use crate::common::{
    Arena, Chunk, ClosureCompileBundle, Instruction, LoxFunInner, LoxFunction,
    LoxString, Position, Upvalue, Value,
};
use crate::scanner::*;

use super::prec::Precedence;
use super::resolver::Resolver;

use lazy_static::lazy_static;

use std::mem;

use log::debug;

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

/// FunctionType helps with addressing syntax errors like return from top-level.
#[derive(Debug, Eq, PartialEq)]
enum FunctionKind {
    // main function
    Script,
    Function,
}

impl std::default::Default for FunctionKind {
    fn default() -> Self {
        FunctionKind::Script
    }
}

/// A helper unit for compiling a function, including components of LoxFunction
/// and a dedicated Resolver.
#[derive(Default)]
struct CompileUnit {
    arity: usize,
    name: String,
    chunk: Chunk,
    kind: FunctionKind,
    resolver: Resolver,
    upvalues: Vec<Upvalue>,
}

impl CompileUnit {
    pub fn end_compile(mut self) -> ClosureCompileBundle {
        self.chunk.push_instr(Instruction::Nil, None);
        self.chunk.push_instr(Instruction::Return, None);
        ClosureCompileBundle {
            function: LoxFunction::new(LoxFunInner::new(
                self.arity, self.name, self.chunk,
            )),
            upvalues: self.upvalues,
        }
    }

    // arity will be modified during parsing function declaration.
    pub fn arity(&mut self, arity: usize) {
        self.arity = arity;
    }

    pub fn name(mut self, name: &str) -> Self {
        self.name = name.to_string();
        self
    }

    pub fn kind(mut self, kind: FunctionKind) -> Self {
        self.kind = kind;
        self
    }

    pub fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
        if is_local {
            self.resolver.mark_captured(index);
        }
        let upvalue = Upvalue { index, is_local };
        match self.upvalues.iter().position(|v| *v == upvalue) {
            Some(x) => x,
            None => {
                self.upvalues.push(upvalue);
                self.upvalues.len() - 1
            }
        }
    }
}

pub struct Compiler<'a> {
    errors: Vec<SyntaxError>,
    peeked: Option<Token>,
    tokens: Scanner<'a>,
    arena: Arena,
    // current working unit
    unit: CompileUnit,
    enclosing_units: Vec<CompileUnit>,
}

impl<'a> Compiler<'a> {
    pub fn new(scanner: Scanner<'a>) -> Compiler<'a> {
        let mut unit = CompileUnit::default();
        unit.name.push_str("__main__");
        Compiler {
            errors: Vec::new(),
            peeked: None,
            tokens: scanner,
            arena: Arena::default(),
            enclosing_units: vec![],
            unit,
        }
    }

    fn begin_unit(&mut self, name: &str, kind: FunctionKind) {
        debug!("begin unit for {:?}", name);
        let mut unit = CompileUnit::default().name(name).kind(kind);
        mem::swap(&mut self.unit, &mut unit);
        self.enclosing_units.push(unit);
    }

    fn end_unit(&mut self) -> ClosureCompileBundle {
        if self.enclosing_units.is_empty() {
            let unit = mem::take(&mut self.unit);
            debug!("end unit for {:?}", unit.name);
            unit.end_compile()
        } else {
            let mut unit = self.enclosing_units.pop().unwrap();
            mem::swap(&mut self.unit, &mut unit);
            debug!("end unit for {:?}", unit.name);
            unit.end_compile()
        }
    }

    fn resolve_upvalues(&mut self, name: &str) -> Option<usize> {
        // transform the recursive version to iterative.
        // a bit messy.
        debug!("search for upvalue {:?} @ {:?}", name, self.unit.name);

        if self.enclosing_units.is_empty() {
            return None;
        }

        if let Some(index) = self
            .enclosing_units
            .last()
            .unwrap()
            .resolver
            .resolve_variale(name)
        {
            debug!(
                " find {:?} in {:?} at slot {} immediately",
                name,
                self.enclosing_units.last().unwrap().name,
                index
            );
            return Some(self.unit.add_upvalue(index, true));
        }

        // reslove local variable at idx - 1 unit
        // capture it into idx unit
        let mut idx = self.enclosing_units.len() - 1;

        // the index of higher upvalue
        let mut index: Option<usize> = None;

        unsafe {
            while idx > 0 {
                if let Some(x) = self
                    .enclosing_units
                    .get_unchecked(idx - 1)
                    .resolver
                    .resolve_variale(name)
                {
                    debug!(
                        " capture {:?} in {:?} at slot {} into {:?}",
                        name,
                        self.enclosing_units.get_unchecked(idx - 1).name,
                        x,
                        self.enclosing_units.get_unchecked(idx).name,
                    );
                    index = Some(
                        self.enclosing_units
                            .get_unchecked_mut(idx)
                            .add_upvalue(x, true),
                    );
                    break;
                }
                idx -= 1;
            }
        }

        // capture something, forword it to the leaf unit
        if let Some(mut index) = index {
            for unit in self.enclosing_units.iter_mut().skip(idx + 1) {
                debug!(
                    " forward upvalue in {:?} -> upper slot {}",
                    unit.name, index
                );
                index = unit.add_upvalue(index, false);
            }
            debug!(
                " finally set upvalue in {:?} -> upper slot {}",
                self.unit.name, index
            );
            return Some(self.unit.add_upvalue(index, false));
        }
        None
    }

    fn advance(&mut self) -> Option<Token> {
        if self.peeked.is_some() {
            self.peeked.take()
        } else {
            // loop to find one valid token
            while let Some(res) = self.tokens.next() {
                match res {
                    Ok(token) => return Some(token),
                    Err(e) => self.errors.push(e.into()),
                }
            }
            None
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

    fn begin_scope(&mut self) {
        self.unit.resolver.begin_scope();
    }

    fn end_scope(&mut self) {
        let n = self.unit.resolver.end_scope();
        self.unit
            .chunk
            .push_instr(Instruction::CloseUpvalue(n), None);
        for _ in 0..n {
            self.emit_pop();
        }
    }

    fn emit_loop(&mut self, start: usize) {
        let offset = self.unit.chunk.code.len() - start + 1;
        self.unit.chunk.push_instr(Instruction::Loop(offset), None);
    }

    fn emit_pop(&mut self) {
        // Pop never fails, it doesn't matter what the position is.
        self.unit.chunk.push_instr(Instruction::Pop, None);
    }

    // Emit a jump instruction, return its index for later patching.
    fn emit_jump(&mut self, instr: Instruction) -> usize {
        // max_value to make program panic if something is going wrong.
        let offset = usize::max_value();
        let instr = match instr {
            Instruction::Jump(_) => Instruction::Jump(offset),
            Instruction::JumpIfFalse(_) => Instruction::JumpIfFalse(offset),
            _ => panic!("wrong arg for emit_jump"),
        };
        self.unit.chunk.push_instr(instr, None);
        self.unit.chunk.code.len() - 1
    }

    // Patch the jump instruction at index, update its offset to the current
    // address.
    fn patch_jump(&mut self, index: usize) {
        let offset = self.unit.chunk.code.len() - index - 1;
        let instr = self.unit.chunk.code.get_mut(index).unwrap();
        *instr = match *instr {
            Instruction::JumpIfFalse(_) => Instruction::JumpIfFalse(offset),
            Instruction::Jump(_) => Instruction::Jump(offset),
            _ => unreachable!(),
        }
    }

    fn emit_instr(
        &mut self,
        instr: Instruction,
        pos: Position,
    ) -> CompileResult<()> {
        self.unit.chunk.push_instr(instr, Some(pos));
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

        let get_op;
        let set_op;
        if let Some(idx) = self.unit.resolver.resolve_variale(tk.as_str()) {
            get_op = Instruction::GetLocal(idx);
            set_op = Instruction::SetLocal(idx);
        } else if let Some(idx) = self.resolve_upvalues(tk.as_str()) {
            if idx >= 255 {
                return Err(SyntaxError::new_compiler_err(
                    Some(tk),
                    "Too many closure variables in function.",
                ));
            }
            get_op = Instruction::GetUpval(idx);
            set_op = Instruction::SetUpval(idx);
        } else {
            let ident = self.arena.alloc_string_ref(tk.as_str());
            get_op = Instruction::GetGlobal(ident.clone());
            set_op = Instruction::SetGlobal(ident);
        }
        // Assignment expression has no handler in `dispatch_prefix`,
        // so we have to tackle with its precedence manually.
        // variable has two references:
        //   1. dispatch_prefix
        //   2. call TODO
        if can_assign && self.advance_if_eq(&EQUAL).is_some() {
            self.expression()?; // more assignment
            self.emit_instr(set_op, tk.position)
        } else {
            self.emit_instr(get_op, tk.position)
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
        if tk.kind == *LESS_EQUAL
            || tk.kind == *GREATER_EQUAL
            || tk.kind == *BANG_EQUAL
        {
            self.emit_instr(Instruction::Not, tk.position)?;
        }
        Ok(())
    }

    fn ternary(&mut self) -> CompileResult<()> {
        self.advance();
        // right-associated:
        // 1>0?10:20?100:101 will expand to 1>0? 10 : (20?100:101) = 10
        // and we don't want this: cond ? A=foo : B=bar

        let then_jump = self.emit_jump(Instruction::JumpIfFalse(0));
        self.emit_pop(); // pop condition value and execute then branch.
        self.parse_with(Precedence::Ternary)?; // then branch.
        let end_jump = self.emit_jump(Instruction::Jump(0));
        self.patch_jump(then_jump);
        self.emit_pop(); // pop condition value and try to execute else branch.

        self.consume_or_err(&COLON, "Expect ':' in ternary expression")?;
        self.parse_with(Precedence::Ternary)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn and(&mut self) -> CompileResult<()> {
        self.advance();
        let end_jump = self.emit_jump(Instruction::JumpIfFalse(0));
        self.emit_pop();
        self.parse_with(Precedence::And)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn or(&mut self) -> CompileResult<()> {
        self.advance();
        let else_jump = self.emit_jump(Instruction::JumpIfFalse(0));
        let end_jump = self.emit_jump(Instruction::Jump(0));
        self.patch_jump(else_jump);
        self.emit_pop();
        self.parse_with(Precedence::Or)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn argument_list(&mut self, tk: &Token) -> CompileResult<usize> {
        let mut arg_count = 0;

        if self.peek_check(|kind| kind != &*RIGHT_PAREN) {
            loop {
                self.expression()?;
                arg_count += 1;
                if arg_count >= 255 {
                    let err = SyntaxError::new_compiler_err(
                        Some(tk.clone()),
                        "Can't have more than 255 arguments.",
                    );
                    self.errors.push(err);
                }
                if self.advance_if_eq(&COMMA).is_none() {
                    break;
                }
            }
        }
        self.consume_or_err(&RIGHT_PAREN, "Expect ')' after parameters.")?;
        Ok(arg_count)
    }

    fn call(&mut self) -> CompileResult<()> {
        let tk = self.advance().unwrap(); // left paren
        let arg_count = self.argument_list(&tk)?;
        self.emit_instr(Instruction::Call(arg_count), tk.position)
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
            Some(&TokenKind::AND) => self.and(),
            Some(&TokenKind::OR) => self.or(),
            Some(&TokenKind::LEFT_PAREN) => self.call(),
            _ => unreachable!("{:?}", self.peek()),
        }
    }

    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    // -----------------   statements parsing   -----------------------------
    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------

    /// Declare a variable, if it is a global variable, return a Some(LoxString),
    /// otherwise, return None
    fn declare_variable(
        &mut self,
        tk: &Token,
    ) -> CompileResult<Option<LoxString>> {
        if self.unit.resolver.is_local_ready() {
            if self.unit.resolver.declare_variable(tk.as_str()) {
                Ok(None)
            } else {
                Err(SyntaxError::new_compiler_err(
                    Some(tk.clone()),
                    "Already variable with this name in this scope.",
                ))
            }
        } else {
            Ok(Some(self.arena.alloc_string_ref(tk.as_str())))
        }
    }

    /// Define a variable, if it is a global variable, emits a Instruction::DefineGlobal,
    /// otherwise, mark the local variable initialized.
    fn define_variable(
        &mut self,
        identifier: Option<LoxString>,
        pos: Position,
    ) -> CompileResult<()> {
        if let Some(identifier) = identifier {
            self.emit_instr(Instruction::DefGlobal(identifier), pos)?;
        } else {
            self.unit.resolver.mark_initialized();
        }
        Ok(())
    }

    fn var_decl(&mut self) -> CompileResult<()> {
        let tk = self.consume_or_err(&IDENTIFIER, "Expect variable name")?;
        let identifier = self.declare_variable(&tk)?;

        // eval the initializer on the stack top
        if self.advance_if_eq(&EQUAL).is_some() {
            self.expression()?;
        } else {
            self.emit_instr(Instruction::Nil, tk.position)?;
        }
        self.consume_or_err(
            &SEMICOLON,
            "Expect ';' after variable declaration.",
        )?;
        self.define_variable(identifier, tk.position)
    }

    fn function(
        &mut self,
        name: &Token,
        kind: FunctionKind,
    ) -> CompileResult<()> {
        self.begin_unit(name.as_str(), kind);
        self.begin_scope(); // create function local scope
        self.consume_or_err(&LEFT_PAREN, "Expect '(' after function name.")?;

        // handle param list
        let mut arity = 0;
        if self.peek_check(|k| k != &*RIGHT_PAREN) {
            loop {
                arity += 1;
                if arity >= 255 {
                    let err = SyntaxError::new_compiler_err(
                        Some(name.clone()),
                        "Can't have more than 255 parameters.",
                    );
                    self.errors.push(err);
                }
                // reserve positions on frame stack for local variables(parameters)
                let tk =
                    self.consume_or_err(&IDENTIFIER, "Expect parameter name.")?;
                let identifier = self.declare_variable(&tk)?;
                self.define_variable(identifier, tk.position)?;
                if self.advance_if_eq(&COMMA).is_none() {
                    break;
                }
            }
        }
        self.unit.arity(arity);

        self.consume_or_err(&RIGHT_PAREN, "Expect ')' after parameters.")?;

        // emit bytecodes of the body to the new unit.
        self.consume_or_err(&LEFT_BRACE, "Expect '{' before function body.")?;
        self.block_stmt()?;

        self.end_scope();
        let bundle = self.end_unit();
        // bundle.function.disassemble();
        self.emit_instr(Instruction::Closure(Box::new(bundle)), name.position)
    }

    fn func_decl(&mut self) -> CompileResult<()> {
        let tk = self.consume_or_err(&IDENTIFIER, "Expect function name")?;
        let identifier = self.declare_variable(&tk)?;
        // mark it initialized early to support recursive call.
        self.unit.resolver.mark_initialized();
        // eval LoxFunction on the stacktop
        self.function(&tk, FunctionKind::Function)?;
        self.define_variable(identifier, tk.position)
    }

    fn print_stmt(&mut self) -> CompileResult<()> {
        self.expression()?;
        let tk = self.consume_or_err(&SEMICOLON, "Expect ';' after value.")?;
        self.emit_instr(Instruction::Print, tk.position)
    }

    fn expression_stmt(&mut self) -> CompileResult<()> {
        self.expression()?;
        self.consume_or_err(&SEMICOLON, "Expect ';' after expression.")?;
        self.emit_pop();
        Ok(())
    }

    fn block_stmt(&mut self) -> CompileResult<()> {
        while self.peek_check(|k| k != &*RIGHT_BRACE) {
            if let Err(err) = self.declaration() {
                self.errors.push(err);
                self.synchronize();
            }
        }
        self.consume_or_err(&RIGHT_BRACE, "Expect '}' after block.")?;
        Ok(())
    }

    fn if_stmt(&mut self) -> CompileResult<()> {
        self.consume_or_err(&LEFT_PAREN, "Expect '(' after 'if'.")?;
        self.expression()?;
        self.consume_or_err(&RIGHT_PAREN, "Expect ')' after condition.")?;
        let then_jump = self.emit_jump(Instruction::JumpIfFalse(0));
        self.emit_pop(); // pop condition value and execute then branch.
        self.statement()?; // then branch.
        let end_jump = self.emit_jump(Instruction::Jump(0));
        self.patch_jump(then_jump);
        self.emit_pop(); // pop condition value and try to execute else branch.

        if self.advance_if_eq(&ELSE).is_some() {
            self.statement()?; // else branch.
        }
        self.patch_jump(end_jump);
        Ok(())
    }

    fn while_stmt(&mut self) -> CompileResult<()> {
        let loop_start = self.unit.chunk.code.len();
        self.consume_or_err(&LEFT_PAREN, "Expect '(' after 'while'.")?;
        self.expression()?;
        self.consume_or_err(&RIGHT_PAREN, "Expect ')' after condition.")?;
        let end_jump = self.emit_jump(Instruction::JumpIfFalse(0));
        self.emit_pop();
        self.statement()?;
        self.emit_loop(loop_start);

        self.patch_jump(end_jump);
        self.emit_pop();
        Ok(())
    }

    fn for_stmt(&mut self) -> CompileResult<()> {
        self.begin_scope();
        self.consume_or_err(&LEFT_PAREN, "Expect '(' after 'for'.")?;

        // initializer clause
        if self.advance_if_eq(&SEMICOLON).is_some() {
        } else if self.advance_if_eq(&VAR).is_some() {
            self.var_decl()?;
        } else {
            self.expression_stmt()?;
        }

        let mut loop_start = self.unit.chunk.code.len();

        // condition clause
        let mut end_jump = None;
        if self.advance_if_eq(&SEMICOLON).is_none() {
            self.expression()?; // leave the value on the stack
            self.consume_or_err(
                &SEMICOLON,
                "Expect ';' after loop condition.",
            )?;
            end_jump = Some(self.emit_jump(Instruction::JumpIfFalse(0)));
            self.emit_pop();
        }

        match self.peek() {
            Some(&TokenKind::RIGHT_PAREN) => (), // no increment
            _ => {
                // condition-true will fall through to the body
                let body_jump = self.emit_jump(Instruction::Jump(0));
                // when body ends, jump to here to execute increment clause
                let inc_start = self.unit.chunk.code.len();
                // increment expression, use like a statement
                self.expression()?;
                self.emit_pop();

                // when inc ends, jump to condition clause.
                self.emit_loop(loop_start);
                loop_start = inc_start;
                self.patch_jump(body_jump);
            }
        }
        self.consume_or_err(&RIGHT_PAREN, "Expect ')' after for clauses.")?;

        self.statement()?;
        self.emit_loop(loop_start);

        if let Some(end_jump) = end_jump {
            self.patch_jump(end_jump);
            self.emit_pop();
        }
        self.end_scope();
        Ok(())
    }

    fn return_stmt(&mut self) -> CompileResult<()> {
        // TODO: clean up the scattered `push_instr` and `emit_instr`
        if self.advance_if_eq(&SEMICOLON).is_some() {
            self.unit.chunk.push_instr(Instruction::Nil, None);
            self.unit.chunk.push_instr(Instruction::Return, None);
        } else {
            self.expression()?;
            self.consume_or_err(&SEMICOLON, "Expect ';' after return value.")?;
            self.unit.chunk.push_instr(Instruction::Return, None);
        }
        Ok(())
    }

    fn statement(&mut self) -> CompileResult<()> {
        if self.advance_if_eq(&PRINT).is_some() {
            self.print_stmt()
        } else if self.advance_if_eq(&IF).is_some() {
            self.if_stmt()
        } else if self.advance_if_eq(&WHILE).is_some() {
            self.while_stmt()
        } else if self.advance_if_eq(&FOR).is_some() {
            self.for_stmt()
        } else if let Some(tk) = self.advance_if_eq(&RETURN) {
            if self.unit.kind == FunctionKind::Script {
                return Err(SyntaxError::new_compiler_err(
                    Some(tk),
                    "Can't return from top-level code.",
                ));
            }
            self.return_stmt()
        } else if self.advance_if_eq(&LEFT_BRACE).is_some() {
            self.begin_scope();
            self.block_stmt()?;
            self.end_scope();
            Ok(())
        } else {
            self.expression_stmt()
        }
    }

    fn declaration(&mut self) -> CompileResult<()> {
        if self.advance_if_eq(&VAR).is_some() {
            self.var_decl()
        } else if self.advance_if_eq(&FUN).is_some() {
            self.func_decl()
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

    pub fn compile(
        &mut self,
    ) -> Result<(ClosureCompileBundle, Arena), Vec<SyntaxError>> {
        while self.peek().is_some() {
            if let Err(e) = self.declaration() {
                self.errors.push(e);
                self.synchronize();
            }
        }
        if self.errors.is_empty() {
            Ok((self.end_unit(), mem::take(&mut self.arena)))
        } else {
            Err(mem::take(&mut self.errors))
        }
    }
}

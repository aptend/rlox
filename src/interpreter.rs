use crate::ast::*;
use crate::scanner::{Token, TokenKind};

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

// it is lame to copy Value in every calculation step during treewalk
// use rc to rescure? but, whatever for now
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    String(String),
    Number(f64),
    Boolean(bool),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Boolean(false) => false,
            _ => true,
        }
    }
}

impl std::default::Default for Value {
    fn default() -> Self {
        Value::Nil
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::String(s) => write!(f, "{}", s),
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
        }
    }
}

// ------------------------------------------------------------------------
// ------- RuntimeError ---------------------------------------------------
// ------------------------------------------------------------------------

type BoxToken = Box<Token>;
type RuntimeResult<T> = Result<T, RuntimeError>;

pub enum RuntimeError {
    UnaryMismatchedType(BoxToken),
    BinaryMismatchedType(BoxToken),
    UndefinedIdentifier(BoxToken),
    BreakControl,
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
                write!(
                    f,
                    "Undefined identifier: {}",
                    token.string_ref().unwrap()
                )
            }
            RuntimeError::BreakControl => write!(f, "break control signal"),
        }
    }
}

// ------------------------------------------------------------------------
// ----- Trait Interpret for expr -----------------------------------------
// ------------------------------------------------------------------------

trait Interpret {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value>;
}

impl Interpret for Expr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        match self {
            Expr::Literal(l) => l.interpret(interpreter),
            Expr::Unary(u) => u.interpret(interpreter),
            Expr::Binary(b) => b.interpret(interpreter),
            Expr::Grouping(g) => g.interpret(interpreter),
            Expr::Variable(v) => v.interpret(interpreter),
            Expr::Assign(a) => a.interpret(interpreter),
            Expr::Logical(l) => l.interpret(interpreter),
        }
    }
}

impl Interpret for AssignExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        let val = self.value.interpret(interpreter)?;
        interpreter.env.assign(&self.name, val.clone())?;
        Ok(val)
    }
}

impl Interpret for VariableExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        interpreter.env.get(&self.name)
    }
}

impl Interpret for Literal {
    fn interpret(&self, _: &mut Interpreter) -> RuntimeResult<Value> {
        match self {
            Literal::Nil => Ok(Value::Nil),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Number(n) => Ok(Value::Number(*n)),
        }
    }
}

impl Interpret for Grouping {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        self.expr.interpret(interpreter)
    }
}

impl Interpret for UnaryExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        let val = self.right.interpret(interpreter)?;
        match &self.op.kind {
            TokenKind::BANG => Ok(Value::Boolean(!val.is_truthy())),
            TokenKind::MINUS => {
                if let Value::Number(n) = val {
                    Ok(Value::Number(-n))
                } else {
                    Err(RuntimeError::UnaryMismatchedType(Box::new(
                        self.op.clone(),
                    )))
                }
            }
            _ => unreachable!(),
        }
    }
}

impl Interpret for BinaryExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        let left_val = self.left.interpret(interpreter)?;
        let right_val = self.right.interpret(interpreter)?;
        match (&self.op.kind, left_val, right_val) {
            (TokenKind::PLUS, Value::String(mut s1), Value::String(s2)) => {
                s1 += &s2;
                Ok(Value::String(s1))
            }
            (TokenKind::PLUS, Value::Number(n1), Value::Number(n2)) => {
                Ok(Value::Number(n1 + n2))
            }
            (TokenKind::MINUS, Value::Number(n1), Value::Number(n2)) => {
                Ok(Value::Number(n1 - n2))
            }
            (TokenKind::STAR, Value::Number(n1), Value::Number(n2)) => {
                Ok(Value::Number(n1 * n2))
            }
            (TokenKind::SLASH, Value::Number(n1), Value::Number(n2)) => {
                Ok(Value::Number(n1 / n2))
            }
            (TokenKind::LESS, Value::Number(n1), Value::Number(n2)) => {
                Ok(Value::Boolean(n1 < n2))
            }
            (TokenKind::LESS_EQUAL, Value::Number(n1), Value::Number(n2)) => {
                Ok(Value::Boolean(n1 <= n2))
            }
            (TokenKind::GREATER, Value::Number(n1), Value::Number(n2)) => {
                Ok(Value::Boolean(n1 > n2))
            }
            (
                TokenKind::GREATER_EQUAL,
                Value::Number(n1),
                Value::Number(n2),
            ) => Ok(Value::Boolean(n1 >= n2)),
            (TokenKind::EQUAL_EQUAL, v1, v2) => Ok(Value::Boolean(v1 == v2)),
            (TokenKind::BANG_EQUAL, v1, v2) => Ok(Value::Boolean(v1 != v2)),
            _ => Err(RuntimeError::BinaryMismatchedType(Box::new(
                self.op.clone(),
            ))),
        }
    }
}

impl Interpret for LogicalExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        let left_branch = self.left.interpret(interpreter)?;
        match &self.op.kind {
            TokenKind::OR => {
                if left_branch.is_truthy() {
                    Ok(left_branch)
                } else {
                    self.right.interpret(interpreter)
                }
            }
            TokenKind::AND => {
                if !left_branch.is_truthy() {
                    Ok(left_branch)
                } else {
                    self.right.interpret(interpreter)
                }
            }
            _ => unreachable!(),
        }
    }
}

// ---------------------------------------------------------------------------
// ---- Trait Execute for Stmt -----------------------------------------------
// ---------------------------------------------------------------------------

trait Execute {
    fn execute(&self, interpreter: &mut Interpreter) -> RuntimeResult<()>;
}

impl Execute for Stmt {
    fn execute(&self, interpreter: &mut Interpreter) -> RuntimeResult<()> {
        match self {
            Stmt::Print(expr) => {
                println!("{}", expr.interpret(interpreter)?);
                Ok(())
            }
            Stmt::Expression(expr) => {
                expr.interpret(interpreter)?;
                Ok(())
            }
            Stmt::Var(v) => {
                let init_val = v.init.interpret(interpreter)?;
                interpreter.env.define(&v.name, init_val)
            }
            Stmt::Block(b) => b.execute(interpreter),
            Stmt::If(i) => i.execute(interpreter),
            Stmt::While(w) => w.execute(interpreter),
            Stmt::Break => Err(RuntimeError::BreakControl),
        }
    }
}

impl Execute for BlockStmt {
    fn execute(&self, interpreter: &mut Interpreter) -> RuntimeResult<()> {
        interpreter.push_new_env();
        for stmt in &self.stmts {
            if let Err(e) = stmt.execute(interpreter) {
                interpreter.pop_env();
                return Err(e);
            }
        }
        interpreter.pop_env();
        Ok(())
    }
}

impl Execute for IfStmt {
    fn execute(&self, interpreter: &mut Interpreter) -> RuntimeResult<()> {
        if self.cond.interpret(interpreter)?.is_truthy() {
            self.taken.execute(interpreter)
        } else if let Some(ref stmt) = self.no_token {
            stmt.execute(interpreter)
        } else {
            Ok(())
        }
    }
}

impl Execute for WhileStmt {
    fn execute(&self, interpreter: &mut Interpreter) -> RuntimeResult<()> {
        while self.cond.interpret(interpreter)?.is_truthy() {
            match self.body.execute(interpreter) {
                Err(RuntimeError::BreakControl) => break,
                Err(e) => return Err(e),
                Ok(_) => (), // nothing to do, loop next
            };
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// ---- Environment ----------------------------------------------------------
// ---------------------------------------------------------------------------

pub struct Environment {
    // multiple nest scopes could refer to the same top level scope
    // they have to hold shared muttablity (in single thread)
    inner: Rc<RefCell<EnvInner>>,
}

pub struct EnvInner {
    pub enclosing: Option<Environment>,
    map: HashMap<String, Value>,
}

impl std::default::Default for EnvInner {
    fn default() -> Self {
        EnvInner {
            enclosing: None,
            map: HashMap::new(),
        }
    }
}

impl std::default::Default for Environment {
    fn default() -> Self {
        Environment {
            inner: Rc::new(RefCell::new(EnvInner::default())),
        }
    }
}

impl std::clone::Clone for Environment {
    fn clone(&self) -> Self {
        Environment {
            inner: self.inner.clone(),
        }
    }
}

impl EnvInner {
    pub fn new(enclosing: Environment, map: HashMap<String, Value>) -> Self {
        EnvInner {
            enclosing: Some(enclosing),
            map,
        }
    }

    pub fn with_enclosing(enclosing: Environment) -> Self {
        EnvInner {
            enclosing: Some(enclosing),
            map: HashMap::new(),
        }
    }

    pub fn define(&mut self, token: &Token, val: Value) -> RuntimeResult<()> {
        self.map.insert(token.string_ref().unwrap().clone(), val);
        Ok(())
    }

    pub fn assign(&mut self, token: &Token, val: Value) -> RuntimeResult<()> {
        match self.map.get_mut(token.string_ref().unwrap()) {
            Some(v) => {
                *v = val;
                Ok(())
            }
            None => match self.enclosing {
                Some(ref env) => env.assign(token, val),
                None => Err(RuntimeError::UndefinedIdentifier(Box::new(
                    token.clone(),
                ))),
            },
        }
    }

    pub fn get(&self, token: &Token) -> RuntimeResult<Value> {
        // only variable expr will call get, it is safe to unwrap
        match self.map.get(token.string_ref().unwrap()) {
            Some(v) => Ok(v.clone()),
            None => match self.enclosing {
                Some(ref env) => env.get(token),
                None => Err(RuntimeError::UndefinedIdentifier(Box::new(
                    token.clone(),
                ))),
            },
        }
    }
}

impl Environment {
    pub fn new(
        enclosing: Environment,
        map: HashMap<String, Value>,
    ) -> Environment {
        Environment {
            inner: Rc::new(RefCell::new(EnvInner::new(enclosing, map))),
        }
    }

    pub fn enclosing_env(&self) -> Option<Environment> {
        self.inner.borrow().enclosing.clone()
    }

    pub fn with_enclosing(enclosing: Environment) -> Environment {
        Environment {
            inner: Rc::new(RefCell::new(EnvInner::with_enclosing(enclosing))),
        }
    }

    pub fn define(&self, token: &Token, val: Value) -> RuntimeResult<()> {
        self.inner.borrow_mut().define(token, val)
    }

    pub fn assign(&self, token: &Token, val: Value) -> RuntimeResult<()> {
        self.inner.borrow_mut().assign(token, val)
    }

    pub fn get(&self, token: &Token) -> RuntimeResult<Value> {
        self.inner.borrow().get(token)
    }
}

// ---------------------------------------------------------------------------
// ---- Interpreter ----------------------------------------------------------
// ---------------------------------------------------------------------------

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment::default(),
        }
    }

    pub fn push_new_env(&mut self) {
        let env = Environment::with_enclosing(self.env.clone());
        self.env = env;
    }

    pub fn pop_env(&mut self) {
        if let Some(prev) = self.env.enclosing_env() {
            self.env = prev;
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> RuntimeResult<()> {
        for stmt in stmts {
            stmt.execute(self)?;
        }
        Ok(())
    }
}

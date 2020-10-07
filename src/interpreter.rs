use crate::ast::*;
use crate::scanner::{Token, TokenKind};

use std::collections::HashMap;
use std::fmt;

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

type BoxToken = Box<Token>;
type RuntimeResult<T> = Result<T, RuntimeError>;

pub enum RuntimeError {
    UnaryMismatchedType(BoxToken),
    BinaryMismatchedType(BoxToken),
    UndefinedIdentifier(BoxToken),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut msg = String::new();
        let token = match self {
            RuntimeError::UnaryMismatchedType(token) => {
                msg.push_str("Operand of unary op '-' must be a number.");
                token
            }
            RuntimeError::BinaryMismatchedType(token) => {
                msg.push_str("Operands must be numbers");
                token
            }
            RuntimeError::UndefinedIdentifier(token) => {
                msg.push_str("Undefined identifier: ");
                msg.push_str(token.string_ref().unwrap());
                token
            }
        };
        let (line, col) = (token.position.line, token.position.column);
        write!(f, "[line {}, column {}] RuntimeError: {}", line, col, msg)
    }
}

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
        }
    }
}

impl Interpret for AssignExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        let val = self.value.interpret(interpreter)?;
        match interpreter.env.get_mut(self.name.string_ref().unwrap()) {
            Some(v) => {
                *v = val.clone();
                Ok(val)
            }
            None => Err(RuntimeError::UndefinedIdentifier(Box::new(
                self.name.clone(),
            ))),
        }
    }
}

impl Interpret for VariableExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        match interpreter.env.get(self.name.string_ref().unwrap()) {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError::UndefinedIdentifier(Box::new(
                self.name.clone(),
            ))),
        }
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
                interpreter
                    .env
                    .insert(v.name.string_ref().unwrap().clone(), init_val);
                Ok(())
            }
        }
    }
}

pub struct Interpreter {
    env: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> RuntimeResult<()> {
        for stmt in stmts {
            stmt.execute(self)?;
        }
        Ok(())
    }
}

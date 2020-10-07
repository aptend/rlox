use crate::ast::*;
use crate::scanner::{Token, TokenKind};

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
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (token, msg) = match self {
            RuntimeError::UnaryMismatchedType(token) => {
                (token, "Operand of unary op '-' must be a number.")
            }
            RuntimeError::BinaryMismatchedType(token) => {
                (token, "Operands must be numbers")
            }
        };
        let (line, col) = (token.position.line, token.position.column);
        write!(f, "[line {}, column {}] RuntimeError: {}", line, col, msg)
    }
}

trait Interpret {
    fn interpret(&self) -> RuntimeResult<Value>;
}

impl Interpret for Expr {
    fn interpret(&self) -> RuntimeResult<Value> {
        match self {
            Expr::Literal(l) => l.interpret(),
            Expr::Unary(u) => u.interpret(),
            Expr::Binary(b) => b.interpret(),
            Expr::Grouping(g) => g.interpret(),
        }
    }
}

impl Interpret for Literal {
    fn interpret(&self) -> RuntimeResult<Value> {
        match self {
            Literal::Nil => Ok(Value::Nil),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Number(n) => Ok(Value::Number(*n)),
        }
    }
}

impl Interpret for Grouping {
    fn interpret(&self) -> RuntimeResult<Value> {
        self.expr.interpret()
    }
}

impl Interpret for Unary {
    fn interpret(&self) -> RuntimeResult<Value> {
        let val = self.right.interpret()?;
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

impl Interpret for Binary {
    fn interpret(&self) -> RuntimeResult<Value> {
        let left_val = self.left.interpret()?;
        let right_val = self.right.interpret()?;
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

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    pub fn interpret(&self, expr: Expr) -> RuntimeResult<Value> {
        expr.interpret()
    }
}

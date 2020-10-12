use super::TokenKind;
use super::{Interpreter, Value};
use super::{RuntimeError, RuntimeResult};
use crate::ast::expr::*;

use super::LoxCallable;

pub trait Interpret {
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
            Expr::Call(c) => c.interpret(interpreter),
        }
    }
}

impl Interpret for CallExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        // check type
        if let Value::Callable(callee) = self.callee.interpret(interpreter)? {
            // check arity
            let (expect, got) = (callee.arity(), self.arguments.len() as u8);
            if expect != got {
                return Err(RuntimeError::ArityMismatch(
                    Box::new(self.pos_tk.clone()),
                    expect,
                    got,
                ));
            }
            // eval args and call
            let mut args = Vec::new();
            for arg in &self.arguments {
                args.push(arg.interpret(interpreter)?);
            }
            match callee.call(interpreter, args) {
                Err(RuntimeError::ReturnControl(v)) | Ok(v) => Ok(v),
                Err(e) => Err(e),
            }
        } else {
            Err(RuntimeError::NonCallable(Box::new(self.pos_tk.clone())))
        }
    }
}

impl Interpret for AssignExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        let val = self.value.interpret(interpreter)?;
        // interpreter.env.assign(&self.name, val.clone())?;
        interpreter.assign_variable(&self.expr_key, &self.name, val.clone())?;
        Ok(val)
    }
}

impl Interpret for VariableExpr {
    fn interpret(&self, interpreter: &mut Interpreter) -> RuntimeResult<Value> {
        // interpreter.env.get(&self.name)
        interpreter.lookup_variable(&self.expr_key, &self.name)
    }
}

impl Interpret for Literal {
    fn interpret(&self, _: &mut Interpreter) -> RuntimeResult<Value> {
        match self {
            Literal::Nil => Ok(Value::Nil),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::String(s) => Ok(Value::new_string(s)),
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
            (TokenKind::PLUS, Value::String(s1), Value::String(s2)) => {
                let mut s = s1.to_string();
                s += &s2;
                Ok(Value::new_string(&s))
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

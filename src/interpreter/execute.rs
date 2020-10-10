use super::Interpret;
use super::{Environment, Interpreter, LoxFunction, Value};
use super::{RuntimeError, RuntimeResult};
use crate::ast::stmt::*;

pub trait Execute {
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
            Stmt::Block(b) => {
                let env = Environment::with_enclosing(interpreter.env.clone());
                execute_block_with_env(b, interpreter, env)
            }
            Stmt::If(i) => i.execute(interpreter),
            Stmt::While(w) => w.execute(interpreter),
            Stmt::Function(f) => f.execute(interpreter),
            Stmt::Break => Err(RuntimeError::BreakControl),
            Stmt::Return(r) => r.execute(interpreter),
        }
    }
}

impl Execute for ReturnStmt {
    fn execute(&self, interpreter: &mut Interpreter) -> RuntimeResult<()> {
        let val = self.value.interpret(interpreter)?;
        Err(RuntimeError::ReturnControl(val))
    }
}

impl Execute for FunctionStmt {
    fn execute(&self, interpreter: &mut Interpreter) -> RuntimeResult<()> {
        let lox_function = Box::new(LoxFunction::new(self.clone()));
        interpreter
            .env
            .define(&self.name, Value::new_callable(lox_function))?;
        Ok(())
    }
}

// setup new env, execute block, restore old env after executing, anyway.
pub fn execute_block_with_env(
    block: &BlockStmt,
    interpreter: &mut Interpreter,
    env: Environment,
) -> RuntimeResult<()> {
    let previous = interpreter.env.clone();
    interpreter.env = env;
    let result = block.execute(interpreter);
    interpreter.env = previous;
    result
}

impl Execute for BlockStmt {
    fn execute(&self, interpreter: &mut Interpreter) -> RuntimeResult<()> {
        for stmt in &self.stmts {
            stmt.execute(interpreter)?
        }
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

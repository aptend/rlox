use super::Interpret;
use super::{Environment, Interpreter, LoxClass, LoxFunction, Value};
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
            Stmt::Class(c) => c.execute(interpreter),
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
        let lox_function = Box::new(LoxFunction::new(
            self.clone(),
            interpreter.env.clone(),
            false,
        ));

        interpreter
            .env
            .define(&self.name, Value::new_callable(lox_function))?;
        Ok(())
    }
}

impl Execute for ClassStmt {
    fn execute(&self, interpreter: &mut Interpreter) -> RuntimeResult<()> {
        let superclass = if let Some(ref supcls) = self.superclass {
            match supcls.interpret(interpreter)? {
                Value::Class(cls) => Some(cls),
                _ => {
                    return Err(RuntimeError::NonClassSuper(Box::new(
                        self.name.clone(),
                    )))
                }
            }
        } else {
            None
        };
        // two-stage variable binding process allows
        // references to the class inside its own methods.
        // TODO: What does this mean?
        interpreter.env.define(&self.name, Value::default())?;

        let methods = self
            .methods
            .iter()
            .map(|m| {
                let name = m.name.as_str().unwrap().to_owned();
                let lox_func = LoxFunction::new(
                    m.clone(),
                    interpreter.env.clone(),
                    name == "init",
                );
                (name, lox_func)
            })
            .collect();

        let lox_class =
            LoxClass::new(self.name.as_str().unwrap(), superclass, methods);

            interpreter
            .env
            .assign(&self.name, Value::Class(lox_class))?;
        Ok(())
    }
}

// setup new env, execute block, restore old env after executing, anyway.
// use this because executing plain block and executing function body block
// require different environments.
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

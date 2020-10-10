mod callable;
mod environment;
mod execute;
mod interpret;
mod runtime_err;
mod value;

use callable::{Callable, LoxCallable, LoxFunction, NativeClock};
use environment::Environment;
use execute::Execute;
use interpret::Interpret;
use runtime_err::{RuntimeError, RuntimeResult};
use value::Value;

use crate::ast::Stmt;
use crate::scanner::{Token, TokenKind};

pub struct Interpreter {
    env: Environment,
    globals: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Environment::default();
        let globals = env.clone();

        globals
            .define(
                &Token::with_kind(TokenKind::IDENTIFIER("clock".to_string())),
                Value::new_callable(Box::new(NativeClock)),
            )
            .expect("Define global failed");

        Interpreter { env, globals }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> RuntimeResult<()> {
        for stmt in stmts {
            stmt.execute(self)?;
        }
        Ok(())
    }
}

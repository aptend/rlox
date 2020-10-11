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

use std::collections::HashMap;

type ExprKey = u64;
type HopCount = usize;

pub struct Interpreter {
    env: Environment,
    globals: Environment,
    scopes_info: HashMap<ExprKey, HopCount>,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Environment::default();
        let globals = env.clone();
        let scopes_info = HashMap::new();

        globals
            .define(
                &Token::with_kind(TokenKind::IDENTIFIER("clock".to_string())),
                Value::new_callable(Box::new(NativeClock)),
            )
            .expect("Define global failed");

        Interpreter {
            env,
            globals,
            scopes_info,
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> RuntimeResult<()> {
        println!("{:?}", self.scopes_info);
        for stmt in stmts {
            stmt.execute(self)?;
        }
        Ok(())
    }

    pub fn add_resolution(&mut self, expr_key: ExprKey, hops: HopCount) {
        self.scopes_info.insert(expr_key, hops);
    }

    fn lookup_variable(
        &mut self,
        expr_key: &ExprKey,
        name: &Token,
    ) -> RuntimeResult<Value> {
        if let Some(hops) = self.scopes_info.get(expr_key) {
            self.env.get_at(*hops, name)
        } else {
            self.globals.get(name)
        }
    }

    fn assign_variable(
        &mut self,
        expr_key: &ExprKey,
        name: &Token,
        value: Value,
    ) -> RuntimeResult<()> {
        if let Some(hops) = self.scopes_info.get(expr_key) {
            self.env.assign_at(*hops, name, value)
        } else {
            self.globals.assign(name, value)
        }
    }
}

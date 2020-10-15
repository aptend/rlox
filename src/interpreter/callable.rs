use std::fmt;
use std::rc::Rc;

use super::{
    Environment, Interpreter, LoxInstance, RuntimeError, RuntimeResult, Token,
    Value,
};

use super::execute::execute_block_with_env;
use crate::ast::FunctionStmt;

pub trait LoxCallable: fmt::Debug {
    fn arity(&self) -> u8;

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> RuntimeResult<Value>;
}

// Clonable trait object
#[derive(Clone)]
pub struct Callable {
    inner: Rc<Box<dyn LoxCallable>>,
}

impl Callable {
    pub fn new(callee: Box<dyn LoxCallable>) -> Callable {
        Callable {
            inner: Rc::new(callee),
        }
    }
}

impl LoxCallable for Callable {
    fn arity(&self) -> u8 {
        self.inner.arity()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> RuntimeResult<Value> {
        self.inner.call(interpreter, args)
    }
}

impl PartialEq for Callable {
    fn eq(&self, _: &Self) -> bool {
        // make compiler happy, we don't need this actually.
        unimplemented!()
    }
}

impl fmt::Debug for Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.inner)
    }
}

/// --------------------------------------------------------------------------
/// ----------- NativeClock implementation -----------------------------------
/// --------------------------------------------------------------------------
use std::time::{SystemTime, UNIX_EPOCH};
pub struct NativeClock;

impl fmt::Debug for NativeClock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn clock>")
    }
}

impl LoxCallable for NativeClock {
    fn arity(&self) -> u8 {
        0
    }

    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> RuntimeResult<Value> {
        Ok(Value::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        ))
    }
}

#[derive(Clone)]
pub struct LoxFunction {
    is_initializer: bool,
    pub func_stmt: FunctionStmt,
    pub closure: Environment,
}

impl LoxFunction {
    pub fn new(
        func_stmt: FunctionStmt,
        closure: Environment,
        is_initializer: bool,
    ) -> Self {
        LoxFunction {
            func_stmt,
            closure,
            is_initializer,
        }
    }

    pub fn bind(&self, inst: LoxInstance) -> Self {
        let bound_env = Environment::with_enclosing(self.closure.clone());
        bound_env
            .define(&Token::new_this(), Value::Instance(inst))
            .unwrap();
        LoxFunction {
            func_stmt: self.func_stmt.clone(),
            closure: bound_env,
            is_initializer: self.is_initializer,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> u8 {
        self.func_stmt.params.len() as u8
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> RuntimeResult<Value> {
        let local_env = Environment::with_enclosing(self.closure.clone());
        for (param, arg) in self.func_stmt.params.iter().zip(args) {
            local_env.define(param, arg)?;
        }
        match execute_block_with_env(
            &self.func_stmt.body,
            interpreter,
            local_env,
        )
        .and(Ok(Value::default()))
        {
            Err(RuntimeError::ReturnControl(v)) | Ok(v) => {
                if self.is_initializer {
                    Ok(self.closure.get_at(0, &Token::new_this()).unwrap())
                } else {
                    Ok(v)
                }
            }
            Err(e) => Err(e),
        }
    }
}

impl fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.func_stmt.name.as_str().unwrap())
    }
}

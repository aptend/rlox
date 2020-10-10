use std::cell::RefCell;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

use super::{Environment, Interpreter, RuntimeResult, Value};

use super::execute::execute_block_with_env;

use crate::ast::{FunctionStmt, Stmt};

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
    inner: Rc<RefCell<Box<dyn LoxCallable>>>,
}

impl Callable {
    pub fn new(callee: Box<dyn LoxCallable>) -> Callable {
        Callable {
            inner: Rc::new(RefCell::new(callee)),
        }
    }
}

impl LoxCallable for Callable {
    fn arity(&self) -> u8 {
        self.inner.borrow().arity()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> RuntimeResult<Value> {
        self.inner.borrow().call(interpreter, args)
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
        write!(f, "{:?}", self.inner.borrow())
    }
}

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

pub struct LoxFunction {
    func_stmt: FunctionStmt,
    closure: Environment,
}

impl LoxFunction {
    pub fn new(func_stmt: FunctionStmt, closure: Environment) -> Self {
        LoxFunction { func_stmt, closure }
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
        if let Stmt::Block(block) = self.func_stmt.body.deref() {
            execute_block_with_env(block, interpreter, local_env)?;
        }
        Ok(Value::default())
    }
}

impl fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.func_stmt.name.string_ref().unwrap())
    }
}

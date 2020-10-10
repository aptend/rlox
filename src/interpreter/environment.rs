use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{RuntimeError, RuntimeResult, Value};
use crate::scanner::Token;

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

use super::value::RcString;
use super::{Interpreter, LoxCallable, RuntimeResult, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub struct LoxClass {
    name: RcString,
}

impl LoxClass {
    pub fn new(name: &str) -> Self {
        LoxClass {
            name: RcString::new(name),
        }
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> u8 {
        0
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _args: Vec<Value>,
    ) -> RuntimeResult<Value> {
        Ok(Value::Instance(LoxInstance::new(self.name.clone())))
    }
}

impl fmt::Debug for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", &*self.name)
    }
}

pub struct InstanceInner {
    class_name: RcString,
    fields: HashMap<String, Value>,
}

#[derive(Clone)]
pub struct LoxInstance {
    inner: Rc<RefCell<InstanceInner>>,
}

impl LoxInstance {
    pub fn new(class_name: RcString) -> Self {
        LoxInstance {
            inner: Rc::new(RefCell::new(InstanceInner {
                class_name,
                fields: HashMap::new(),
            })),
        }
    }
}

impl fmt::Debug for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{} instance>", &*self.inner.borrow().class_name)
    }
}

impl PartialEq for LoxInstance {
    fn eq(&self, _: &Self) -> bool {
        // make compiler happy, we don't need this actually.
        unimplemented!()
    }
}

use super::{
    Environment, Interpreter, LoxCallable, LoxFunction, RuntimeResult, Value,
};
use crate::ast::ClassStmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub struct LoxClass {
    inner: Rc<ClassInner>,
}

impl std::ops::Deref for LoxClass {
    type Target = ClassInner;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
pub struct ClassInner {
    name: String,
    methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(cls_stmt: &ClassStmt, env: Environment) -> Self {
        let name = cls_stmt.name.as_str().unwrap().to_owned();

        let methods = cls_stmt
            .methods
            .iter()
            .map(|m| {
                (
                    m.name.as_str().unwrap().to_owned(),
                    LoxFunction::new(m.clone(), env.clone()),
                )
            })
            .collect();

        LoxClass {
            inner: Rc::new(ClassInner { name, methods }),
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
        Ok(Value::Instance(LoxInstance::new(self.clone())))
    }
}

impl fmt::Debug for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", &*self.name)
    }
}

pub struct InstanceInner {
    class: LoxClass,
    fields: HashMap<String, Value>,
}

#[derive(Clone)]
pub struct LoxInstance {
    inner: Rc<RefCell<InstanceInner>>,
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        LoxInstance {
            inner: Rc::new(RefCell::new(InstanceInner {
                class,
                fields: HashMap::new(),
            })),
        }
    }

    fn find_method(&self, name: &str) -> Option<Value> {
        self.inner
            .borrow()
            .class
            .methods
            .get(name)
            .map(|func| Value::new_callable(Box::new(func.bind(self.clone()))))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.inner
            .borrow()
            .fields
            .get(name)
            .cloned()
            .or_else(|| self.find_method(name))
    }

    pub fn set(&self, name: &str, value: Value) {
        self.inner
            .borrow_mut()
            .fields
            .insert(name.to_owned(), value);
    }
}

impl fmt::Debug for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{} instance>", &*self.inner.borrow().class.name)
    }
}

impl PartialEq for LoxInstance {
    fn eq(&self, _: &Self) -> bool {
        // make compiler happy, we don't need this actually.
        unimplemented!()
    }
}

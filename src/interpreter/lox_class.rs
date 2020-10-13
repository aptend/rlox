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
                    // check init name here
                    LoxFunction::new(m.clone(), env.clone()),
                )
            })
            .collect();

        LoxClass {
            inner: Rc::new(ClassInner { name, methods }),
        }
    }

    fn find_method(&self, name: &str) -> Option<&LoxFunction> {
        self.methods.get(name)
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> u8 {
        if let Some(initializer) = self.find_method("init") {
            initializer.arity()
        } else {
            0
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> RuntimeResult<Value> {
        let instance = LoxInstance::new(self.clone());
        if let Some(initializer) = self.find_method("init") {
            initializer.bind(instance.clone()).call(interpreter, args)?;
        }
        Ok(Value::Instance(instance))
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

    fn find_bound_method(&self, name: &str) -> Option<Box<dyn LoxCallable>> {
        self.inner.borrow().class.find_method(name).map(|func| {
            Box::new(func.bind(self.clone())) as Box<dyn LoxCallable>
        })
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.inner
            .borrow()
            .fields
            .get(name)
            .cloned()
            .or_else(|| self.find_bound_method(name).map(Value::new_callable))
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

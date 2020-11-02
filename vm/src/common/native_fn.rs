use super::Value;

use std::fmt;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

std::thread_local! {
    pub static NATIVECLOCK: NativeFunction = NativeFunction(Rc::new(Native {
        name: "clock".to_owned(),
        arity: 0,
        code: clock,
    }));
}

#[derive(Clone)]
pub struct NativeFunction(Rc<Native>);

impl NativeFunction {
    pub fn name(&self) -> &str {
        &self.0.name
    }
    pub fn arity(&self) -> usize {
        self.0.arity
    }
    pub fn code(&self) -> &fn(&[Value]) -> Value {
        &self.0.code
    }
}

pub struct Native {
    pub name: String,
    pub arity: usize,
    pub code: fn(&[Value]) -> Value,
}

impl std::cmp::PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {}>", self.0.name)
    }
}

pub fn clock(_args: &[Value]) -> Value {
    Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}

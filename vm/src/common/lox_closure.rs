use super::LoxFunction;

use std::fmt;
use std::rc::Rc;

#[derive(PartialEq, Eq)]
pub struct Upvalue {
    /// true if is captured from local variable,
    /// otherwise, it is from another upvalue
    pub is_local: bool,
    /// its index in local variable list or upvalue list
    pub index: usize,
}

pub struct Closure {
    function: LoxFunction,
}

#[derive(Clone)]
pub struct LoxClosure(Rc<Closure>);

impl LoxClosure {
    pub fn new(function: LoxFunction) -> Self {
        LoxClosure(Rc::new(Closure { function }))
    }

    #[inline(always)]
    pub fn function(&self) -> &LoxFunction {
        &self.0.function
    }
}

impl std::cmp::PartialEq for LoxClosure {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl fmt::Display for LoxClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure @ {}>", self.0.function)
    }
}

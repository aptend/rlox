use super::LoxFunction;

use std::rc::Rc;
use std::fmt;


pub struct Closure {
    function: LoxFunction,
}

#[derive(Clone)]
pub struct LoxClosure(Rc<Closure>);

impl LoxClosure {
    pub fn new(function: LoxFunction) -> Self {
        LoxClosure(Rc::new(Closure {
            function
        }))
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

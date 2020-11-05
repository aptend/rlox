use super::{LoxFunction, Value};

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

/// Upvalue information comes from Compiling stage
#[derive(PartialEq, Eq)]
pub struct Upvalue {
    /// true if is captured from local variable,
    /// otherwise, it is from another upvalue
    pub is_local: bool,
    /// its index in local variable list or upvalue list
    pub index: usize,
}

impl fmt::Display for Upvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_local {
            write!(f, "local@{}", self.index)
        } else {
            write!(f, "upvalue@{}", self.index)
        }
    }
}

pub struct ClosureCompileBundle {
    pub function: LoxFunction,
    pub upvalues: Vec<Upvalue>,
}

impl fmt::Display for ClosureCompileBundle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.function)?;
        if !self.upvalues.is_empty() {
            write!(f, " with")?;
            for v in &self.upvalues {
                write!(f, " {}", v)?;
            }
        }
        Ok(())
    }
}

/// UpvalueCell serves on runtime stage.
/// It has two states: open and closed, pointing to a stack Value and heap Value respectively
// TODO: Everytime we access UpvalueCell, we have to branch on it is state,
// this will slow down the program. Do the same thing in clox using unsafe.
pub enum UpvalueCell {
    Open(usize),
    Closed(RefCell<Value>),
}
pub struct Closure {
    function: LoxFunction,
    upvalues: Vec<Rc<UpvalueCell>>,
}

/// LoxFuntion equipped with UpvalueCells turns into LoxClosure in runtime.
#[derive(Clone)]
pub struct LoxClosure(Rc<Closure>);

impl LoxClosure {
    pub fn new(function: LoxFunction, upvalues: Vec<Rc<UpvalueCell>>) -> Self {
        LoxClosure(Rc::new(Closure { function, upvalues }))
    }

    #[inline(always)]
    pub fn function(&self) -> &LoxFunction {
        &self.0.function
    }

    #[inline(always)]
    pub fn upvalues(&self) -> &[Rc<UpvalueCell>] {
        &self.0.upvalues
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

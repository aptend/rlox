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

// This is my first failed try. Helpful for understanding how ObjUpvalue works
// #[derive(Clone)]
// enum UpvalueCellAnother {
//     Open(usize),
//     Closed(Rc<RefCell<Value>>)
// }

/// UpvalueCell serves on runtime stage.
/// It has two states: open and closed, pointing to a stack Value and heap Value respectively
// TODO: Everytime we access UpvalueCell, we have to branch on it is state,
// this will slow down the program. Do the same thing in clox using unsafe.
#[derive(Clone)]
pub struct UpvalueCell(Rc<RefCell<CellState>>);

impl UpvalueCell {
    pub fn new_open_with_index(index: usize) -> Self {
        let inner = Rc::new(RefCell::new(CellState::Open(index)));
        UpvalueCell(inner)
    }

    pub fn close_with_value(&self, value: Value) {
        self.0.replace(CellState::Closed(value));
    }

    pub fn index(&self) -> usize {
        match &*self.0.borrow() {
            CellState::Open(idx) => *idx,
            _ => panic!("upvalue cell panic"),
        }
    }
}

impl std::ops::Deref for UpvalueCell {
    type Target = RefCell<CellState>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Debug for UpvalueCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.0.borrow() {
            CellState::Open(x) => write!(f, "open({})", x),
            _ => write!(f, "closed"),
        }
    }
}

pub enum CellState {
    Open(usize),
    Closed(Value),
}
pub struct Closure {
    function: LoxFunction,
    upvalues: Vec<UpvalueCell>,
}

/// LoxFuntion equipped with UpvalueCells turns into LoxClosure in runtime.
#[derive(Clone)]
pub struct LoxClosure(Rc<Closure>);

impl LoxClosure {
    pub fn new(function: LoxFunction, upvalues: Vec<UpvalueCell>) -> Self {
        LoxClosure(Rc::new(Closure { function, upvalues }))
    }

    #[inline(always)]
    pub fn function(&self) -> &LoxFunction {
        &self.0.function
    }

    #[inline(always)]
    pub fn upvalues(&self) -> &[UpvalueCell] {
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

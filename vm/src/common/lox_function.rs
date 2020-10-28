use super::Chunk;

use std::fmt;
use std::rc::Rc;

#[derive(Default)]
pub struct LoxFunInner {
    arity: usize,
    // FIXME: why not give the top-level function a name, like '__main__'?
    name: Option<String>,
    chunk: Chunk,
}

impl LoxFunInner {
    pub fn new(arity: usize, name: Option<String>, chunk: Chunk) -> Self {
        LoxFunInner { arity, name, chunk }
    }
}

/// TODO: Use Rc for a while, homebrewed Gc smart pointer will replace Rc later
#[derive(Clone)]
pub struct LoxFunction(Rc<LoxFunInner>);

impl LoxFunction {
    pub fn new(inner: LoxFunInner) -> Self {
        LoxFunction(Rc::new(inner))
    }

    pub fn disassemble(&self) {
        println!("=======  {}  ========", self);
        self.0.chunk.disassemble();
    }

    #[inline(always)]
    pub fn chunk(&self) -> &Chunk {
        &self.0.chunk
    }

    #[inline(always)]
    pub fn arity(&self) -> usize {
        self.0.arity
    }
}

impl std::cmp::PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0.name {
            Some(s) => write!(f, "<fn {}>", s),
            None => write!(f, "__main__"),
        }
    }
}

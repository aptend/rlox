use crate::common::Position;

pub enum Instruction {
    LoadConstant(Constant),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Return,
}

pub enum Constant {
    Number(f64),
}

pub struct Chunk {
    pub code: Vec<Instruction>,
    pub postions: Vec<Position>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            postions: vec![],
        }
    }

    pub fn push_instr(&mut self, instr: Instruction, pos: Position) {
        self.code.push(instr);
        self.postions.push(pos);
    }
}

use std::fmt;

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


impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::LoadConstant(c) => {
                write!(f, "OP_LoadConstant")
            }
            Instruction::Negate => write!(f, "OP_Negate"),
            Instruction::Add => write!(f, "OP_Add"),
            Instruction::Subtract => write!(f, "OP_Subtract"),
            Instruction::Multiply => write!(f, "OP_Multiply"),
            Instruction::Divide => write!(f, "OP_Divide"),
            Instruction::Return => write!(f, "OP_Return"),
        }
    }
}

pub struct Chunk {
    pub name: String,
    pub code: Vec<Instruction>,
    pub postions: Vec<Position>,
}

impl Chunk {
    pub fn new(name: &str) -> Self {
        Chunk {
            name: name.to_string(),
            code: vec![],
            postions: vec![],
        }
    }

    pub fn push_instr(&mut self, instr: Instruction, pos: Position) {
        self.code.push(instr);
        self.postions.push(pos);
    }

    pub fn dis(&self) {
        println!("====== {} ======", self.name);

    }
}

use std::fmt;

use crate::common::{Position, Value};

// Instructions run in a virtual machine, 16 byets
pub enum Instruction {
    LoadConstant(Value),
    Negate,
    Not,

    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    Less,
    Greater,
    Ternary,

    Nil,
    False,
    True,

    Return,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::LoadConstant(c) => {
                write!(f, "{:20} {}", "OP_LoadConstant", c)
            }
            Instruction::Negate => write!(f, "OP_Negate"),
            Instruction::Not => write!(f, "OP_Not"),
            Instruction::Add => write!(f, "OP_Add"),
            Instruction::Subtract => write!(f, "OP_Subtract"),
            Instruction::Multiply => write!(f, "OP_Multiply"),
            Instruction::Divide => write!(f, "OP_Divide"),
            Instruction::Equal => write!(f, "OP_Equal"),
            Instruction::Less => write!(f, "OP_Less"),
            Instruction::Greater => write!(f, "OP_Greater"),
            Instruction::Ternary => write!(f, "OP_Ternary"),
            Instruction::Nil => write!(f, "OP_LoadNil"),
            Instruction::True => write!(f, "OP_LoadTrue"),
            Instruction::False => write!(f, "OP_LoadFalse"),
            Instruction::Return => write!(f, "OP_Return"),
        }
    }
}

#[derive(Default)]
pub struct Chunk {
    pub name: String,
    pub code: Vec<Instruction>,
    pub positions: Vec<Position>,
}

impl Chunk {
    pub fn new(name: &str) -> Self {
        Chunk {
            name: name.to_string(),
            code: vec![],
            positions: vec![],
        }
    }

    pub fn push_instr(&mut self, instr: Instruction, pos: Position) {
        self.code.push(instr);
        self.positions.push(pos);
    }

    pub fn disassemble(&self) {
        println!("===== {} =====\n", self.name);
        for offset in 0..self.code.len() {
            self.dis_instr(offset);
        }
    }

    fn dis_instr(&self, offset: usize) {
        let line = self.positions[offset].line;
        if offset > 0 && line == self.positions[offset - 1].line {
            println!("{:04}    | {}", offset, self.code[offset]);
        } else if line == 0 {
            // not instr from token, like the last return of vm
            println!("{:04}    * {}", offset, self.code[offset]);
        } else {
            println!("{:04} {:>4} {}", offset, line, self.code[offset]);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_size_of_value() {
        assert_eq!(16, std::mem::size_of::<Value>());
    }

    #[test]
    fn test_dis() {
        let mut chunk = Chunk::new("test program");
        chunk.push_instr(
            Instruction::LoadConstant(Value::Number(1.2)),
            Position::new(123, 1),
        );
        chunk.push_instr(
            Instruction::LoadConstant(Value::Number(3.2)),
            Position::new(123, 9),
        );

        chunk.push_instr(Instruction::Add, Position::new(123, 5));
        chunk.push_instr(Instruction::Return, Position::new(124, 1));
        chunk.disassemble();
        // ===== test program =====
        // 0000  123 OP_LoadConstant      1.2
        // 0001    | OP_LoadConstant      3.2
        // 0002    | OP_Add
        // 0003  124 OP_Return
    }
}

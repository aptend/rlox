use std::fmt;

use super::{ClosureCompileBundle, LoxString, Position, Value};

// Instructions run in a virtual machine, 16 byets
pub enum Instruction {
    LoadConstant(Value),
    Negate,
    Not,

    // we have to packet up LoxFunction and Upvalues generated at compile-time
    // because we no constants list. They are the raw materials for building
    // a closure at runtime.
    Closure(Box<ClosureCompileBundle>),
    GetUpval(usize),
    SetUpval(usize),
    DefGlobal(LoxString),
    GetGlobal(LoxString),
    SetGlobal(LoxString),
    GetLocal(usize),
    SetLocal(usize),
    Print,
    Call(usize),

    Jump(usize),
    JumpIfFalse(usize),
    Loop(usize),

    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    Less,
    Greater,

    Nil,
    False,
    True,

    Pop,
    Return,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Closure(fun) => {
                write!(f, "{:20} {}", "OP_Closure", fun)
            }
            Instruction::DefGlobal(s) => {
                write!(f, "{:20} {:?}", "OP_DefineGlobal", s)
            }
            Instruction::GetGlobal(s) => {
                write!(f, "{:20} {:?}", "OP_GetGlobal", s)
            }
            Instruction::SetGlobal(s) => {
                write!(f, "{:20} {:?}", "OP_SetGlobal", s)
            }
            Instruction::GetLocal(i) => {
                write!(f, "{:20} {:?}", "OP_GetLocal", i)
            }
            Instruction::SetLocal(i) => {
                write!(f, "{:20} {:?}", "OP_SetLocal", i)
            }
            Instruction::GetUpval(i) => {
                write!(f, "{:20} {:?}", "OP_GetUpvalue", i)
            }
            Instruction::SetUpval(i) => {
                write!(f, "{:20} {:?}", "OP_SetUpvalue", i)
            }
            Instruction::LoadConstant(c) => {
                write!(f, "{:20} ", "OP_LoadConstant")?;
                if let Value::String(s) = c {
                    write!(f, "{:?}", s)
                } else {
                    write!(f, "{}", c)
                }
            }
            Instruction::Call(n) => {
                write!(f, "{:20} arg_count:{}", "OP_Call", n)
            }
            Instruction::Jump(n) => write!(f, "{:20} +{}", "OP_Jump", n + 1),
            Instruction::JumpIfFalse(n) => {
                write!(f, "{:20} +{}", "OP_Jump_False", n + 1)
            }
            Instruction::Loop(n) => write!(f, "{:20} -{}", "OP_Loop", n - 1),
            Instruction::Print => write!(f, "OP_Print"),
            Instruction::Negate => write!(f, "OP_Negate"),
            Instruction::Not => write!(f, "OP_Not"),
            Instruction::Add => write!(f, "OP_Add"),
            Instruction::Subtract => write!(f, "OP_Subtract"),
            Instruction::Multiply => write!(f, "OP_Multiply"),
            Instruction::Divide => write!(f, "OP_Divide"),
            Instruction::Equal => write!(f, "OP_Equal"),
            Instruction::Less => write!(f, "OP_Less"),
            Instruction::Greater => write!(f, "OP_Greater"),
            Instruction::Nil => write!(f, "OP_LoadNil"),
            Instruction::True => write!(f, "OP_LoadTrue"),
            Instruction::False => write!(f, "OP_LoadFalse"),
            Instruction::Return => write!(f, "OP_Return"),
            Instruction::Pop => write!(f, "OP_Pop"),
        }
    }
}

#[derive(Default)]
pub struct Chunk {
    pub code: Vec<Instruction>,
    pub positions: Vec<Position>,
}

impl Chunk {
    pub fn push_instr(&mut self, instr: Instruction, pos: Option<Position>) {
        self.code.push(instr);
        match pos {
            Some(pos) => self.positions.push(pos),
            None if self.positions.is_empty() => {
                self.positions.push(Position::default())
            }
            None => self.positions.push(*self.positions.last().unwrap()),
        }
    }

    pub fn disassemble(&self) {
        for offset in 0..self.code.len() {
            self.dis_instr(offset);
        }
    }

    pub fn dis_instr(&self, offset: usize) {
        let line = self.positions[offset].line;
        if offset > 0 && line == self.positions[offset - 1].line {
            println!("{:04}    | {}", offset, self.code[offset]);
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
            Some(Position::new(123, 1)),
        );
        chunk.push_instr(
            Instruction::LoadConstant(Value::Number(3.2)),
            Some(Position::new(123, 9)),
        );

        chunk.push_instr(Instruction::Add, Some(Position::new(123, 5)));
        chunk.push_instr(Instruction::Return, Some(Position::new(124, 1)));
        chunk.disassemble();
        // ===== test program =====
        // 0000  123 OP_LoadConstant      1.2
        // 0001    | OP_LoadConstant      3.2
        // 0002    | OP_Add
        // 0003  124 OP_Return
    }
}

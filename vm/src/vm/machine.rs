use super::value::Value;
use crate::chunk::{Chunk, Constant, Instruction};

use crate::common::Position;

pub struct RunningMachine<'a> {
    code: &'a [Instruction],
    ip: usize,
    stack: Vec<Value>,
}

impl<'a> RunningMachine<'a> {
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }
    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    pub fn run(&mut self) {
        macro_rules! binary_op {
            ($op: tt) => {
                match (self.pop(), self.pop()) {
                    (Value::Number(a), Value::Number(b)) => self.push(Value::Number(b $op a))
                }
            };
        }
        loop {
            let instr = &self.code[self.ip];
            match instr {
                Instruction::Return => {
                    println!("{}", self.pop());
                    return;
                }
                Instruction::Negate => match self.pop() {
                    Value::Number(f) => self.push(Value::Number(-f)),
                },
                Instruction::LoadConstant(c) => self.push(c.into()),
                Instruction::Add => binary_op!(+),
                Instruction::Subtract => binary_op!(-),
                Instruction::Multiply => binary_op!(*),
                Instruction::Divide => binary_op!(/),
            }
        }
    }
}

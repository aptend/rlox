use crate::chunk::{Chunk, Instruction};

use crate::common::{Position, Value};

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
                    (Value::Number(a), Value::Number(b)) => self.push(Value::Number(b $op a)),
                    _ => unimplemented!()
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
                    _ => panic!("TypeError for OP_Negate."),
                },
                Instruction::LoadConstant(c) => self.push(c.clone()),
                Instruction::Add => binary_op!(+),
                Instruction::Subtract => binary_op!(-),
                Instruction::Multiply => binary_op!(*),
                Instruction::Divide => binary_op!(/),
                Instruction::Ternary => {
                    let left = self.pop();
                    let right = self.pop();
                    if self.pop().is_truthy() {
                        self.push(left);
                    } else {
                        self.push(right);
                    }
                }
            }
        }
    }
}

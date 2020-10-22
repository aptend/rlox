use crate::chunk::{Chunk, Instruction};
use crate::common::Value;
use crate::common::Position;

use super::error::RuntimeError;

type VmResult<T> = Result<T, RuntimeError>;

const STACK_MAX: usize = 256;

pub struct Machine<'a> {
    code: &'a [Instruction],
    positions: &'a [Position],
    ip: usize,
    stack: Vec<Value>,
}

impl<'a> Machine<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Machine {
            code: &chunk.code,
            positions: &chunk.positions,
            ip: 0,
            stack: vec![],
        }
    }

    fn push(&mut self, value: Value) {
        if self.stack.len() > STACK_MAX {
            panic!("stack overflow");
        }
        self.stack.push(value);
    }

    
    fn _peek_at(&self, distance: usize) -> &Value {
        let idx = self.stack.len() - distance - 1;
        &self.stack[idx]
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack underflow")
    }

    fn runtime_err(&self, msg: &str) -> VmResult<()> {
        let pos = self.positions[self.ip-1];
        Err(RuntimeError::new(pos, msg))
    }

    pub fn run(&mut self) -> VmResult<()> {
        macro_rules! binary_op {
            ($op: tt) => {
                match (self.pop(), self.pop()) {
                    (Value::Number(a), Value::Number(b)) => self.push(Value::Number(b $op a)),
                    _ => return self.runtime_err("Operands must be numbers."),
                }
            }
        }
        loop {
            let instr = &self.code[self.ip];
            self.ip += 1;
            match instr {
                Instruction::Return => {
                    println!("{}", self.pop());
                    return Ok(());
                }
                Instruction::Negate => match self.pop() {
                    Value::Number(f) => self.push(Value::Number(-f)),
                    _ => return self.runtime_err("Operand must be a number."),
                },
                Instruction::LoadConstant(c) => self.push(c.clone()),
                Instruction::Add => binary_op!(+),
                Instruction::Subtract => binary_op!(-),
                Instruction::Multiply => binary_op!(*),
                Instruction::Divide => binary_op!(/),
                Instruction::Ternary => {
                    // TODO: jump execution
                    let right = self.pop();
                    let left = self.pop();
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

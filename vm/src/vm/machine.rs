use crate::chunk::{Chunk, Instruction};
use crate::common::Position;
use crate::common::Value;

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
        let pos = self.positions[self.ip - 1];
        Err(RuntimeError::new(pos, msg))
    }

    pub fn run(&mut self) -> VmResult<()> {
        // TODO: check_value_on_stack.or_else(raise RuntimeError)
        //       before popping it from stack, like the book did
        macro_rules! binary_op {
            ($typ: tt, $op: tt) => {
                match (self.pop(), self.pop()) {
                    (Value::Number(a), Value::Number(b)) => self.push(Value::$typ(b $op a)),
                    _ => return self.runtime_err("Operands must be numbers."),
                }
            }
        }
        loop {
            // for debug
            // println!("{:?}", self.stack);
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
                Instruction::Not => {
                    let val = !self.pop().is_truthy();
                    self.push(Value::Boolean(val))
                }
                Instruction::LoadConstant(c) => self.push(c.clone()),
                Instruction::Nil => self.push(Value::Nil),
                Instruction::True => self.push(Value::Boolean(true)),
                Instruction::False => self.push(Value::Boolean(false)),
                Instruction::Add => {
                    match (self.pop(), self.pop()) {
                        (Value::Number(b), Value::Number(a)) => {
                            self.push(Value::Number(a + b))
                        }
                        (Value::String(b), Value::String(a)) => {
                            let a: String = a.to_string() + &b;
                            self.push(Value::String(a.into()));
                        }
                        _ => {
                            return self.runtime_err(
                                "Operands must be two numbers or two strings.",
                            )
                        }
                    }
                }
                Instruction::Subtract => binary_op!(Number, -),
                Instruction::Multiply => binary_op!(Number, *),
                Instruction::Divide => binary_op!(Number, /),
                Instruction::Less => binary_op!(Boolean, <),
                Instruction::Greater => binary_op!(Boolean, >),
                Instruction::Equal => {
                    let (a, b) = (self.pop(), self.pop());
                    self.push(Value::Boolean(a == b))
                }
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

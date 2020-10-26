use crate::chunk::{Chunk, Instruction};
use crate::common::{Arena, Position, Value};

use super::error::RuntimeError;

use std::fmt::{format, Arguments};
use std::format_args as args;

type VmResult<T> = Result<T, RuntimeError>;

const STACK_MAX: usize = 256;

pub struct Machine<'a> {
    code: &'a [Instruction],
    positions: &'a [Position],
    arena: Arena,
    ip: usize,
    stack: Vec<Value>,
}

impl<'a> Machine<'a> {
    pub fn new(chunk: &'a Chunk, arena: Arena) -> Self {
        Machine {
            code: &chunk.code,
            positions: &chunk.positions,
            arena,
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

    fn peek(&self, distance: usize) -> &Value {
        let idx = self.stack.len() - distance - 1;
        &self.stack[idx]
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack underflow")
    }

    fn runtime_err(&self, args: Arguments) -> VmResult<()> {
        let pos = self.positions[self.ip - 1];
        Err(RuntimeError::new(pos, format(args)))
    }

    pub fn run(&mut self) -> VmResult<()> {
        // TODO: check_value_on_stack.or_else(raise RuntimeError)
        //       before popping it from stack, like the book did
        macro_rules! binary_op {
            ($typ: tt, $op: tt) => {
                match (self.pop(), self.pop()) {
                    (Value::Number(a), Value::Number(b)) => self.push(Value::$typ(b $op a)),
                    _ => return self.runtime_err(args!("Operands must be numbers.")),
                }
            }
        }
        loop {
            let instr = &self.code[self.ip];
            // for debug
            // println!("{}", instr);
            // println!(" {:?}\n", self.stack);
            self.ip += 1;
            match instr {
                Instruction::Return => {
                    // println!("{}", self.pop());
                    return Ok(());
                }
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::Jump(offset) => {
                    self.ip += offset;
                }
                Instruction::JumpIfFalse(offset) => {
                    if !self.peek(0).is_truthy() {
                        self.ip += offset;
                    }
                }
                Instruction::Loop(offset) => {
                    self.ip -= offset;
                }
                Instruction::Print => {
                    println!("{}", self.pop());
                }
                Instruction::DefGlobal(key) => {
                    let val = self.pop();
                    self.arena.set_global(key.clone(), val);
                }
                Instruction::GetGlobal(key) => {
                    let val = match self.arena.get_global(key) {
                        Some(v) => v,
                        None => {
                            return self.runtime_err(args!(
                                "Undefined variable {:?}.",
                                key
                            ));
                        }
                    };
                    self.push(val);
                }
                Instruction::SetGlobal(key) => {
                    if self.arena.get_global(key).is_some() {
                        self.arena
                            .set_global(key.clone(), self.peek(0).clone());
                    } else {
                        return self.runtime_err(args!(
                            "Undefined variable {:?}.",
                            key
                        ));
                    }
                }
                Instruction::GetLocal(idx) => {
                    let val = self.stack[*idx].clone();
                    self.push(val);
                }
                Instruction::SetLocal(idx) => {
                    self.stack[*idx] = self.peek(0).clone();
                }
                Instruction::Negate => match self.pop() {
                    Value::Number(f) => self.push(Value::Number(-f)),
                    _ => {
                        return self
                            .runtime_err(args!("Operand must be a number."))
                    }
                },
                Instruction::Not => {
                    let val = !self.pop().is_truthy();
                    self.push(Value::Boolean(val))
                }
                Instruction::LoadConstant(c) => self.push(c.clone()),
                Instruction::Nil => self.push(Value::Nil),
                Instruction::True => self.push(Value::Boolean(true)),
                Instruction::False => self.push(Value::Boolean(false)),
                Instruction::Add => match (self.pop(), self.pop()) {
                    (Value::Number(b), Value::Number(a)) => {
                        self.push(Value::Number(a + b))
                    }
                    (Value::String(b), Value::String(a)) => {
                        let a: String = a.to_string() + &b;
                        let val = Value::String(self.arena.alloc_string(a));
                        self.push(val);
                    }
                    _ => {
                        return self.runtime_err(args!(
                            "Operands must be two numbers or two strings.",
                        ));
                    }
                },
                Instruction::Subtract => binary_op!(Number, -),
                Instruction::Multiply => binary_op!(Number, *),
                Instruction::Divide => binary_op!(Number, /),
                Instruction::Less => binary_op!(Boolean, <),
                Instruction::Greater => binary_op!(Boolean, >),
                Instruction::Equal => {
                    let (a, b) = (self.pop(), self.pop());
                    self.push(Value::Boolean(a == b))
                }
            }
        }
    }
}

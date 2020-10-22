use crate::chunk::Instruction;
use crate::common::Value;

const STACK_MAX: usize = 256;

pub struct Machine<'a> {
    code: &'a [Instruction],
    ip: usize,
    stack: Vec<Value>,
}

impl<'a> Machine<'a> {
    pub fn new(code: &'a [Instruction]) -> Self {
        Machine {
            code,
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
    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack underflow")
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
            self.ip += 1;
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

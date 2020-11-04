use crate::common::{
    Arena, ClosureCompileBundle, Instruction, LoxClosure, Position, Value,
    NATIVECLOCK,
};

use super::error::RuntimeError;

use std::fmt::{format, Arguments};
use std::format_args as args;

type VmResult<T> = Result<T, RuntimeError>;

const STACK_MAX: usize = 256;

struct CallFrame {
    closure: LoxClosure,
    // instruction pointer
    ip: usize,
    // frame pointer
    fp: usize,
}

impl CallFrame {
    pub fn new(closure: LoxClosure, ip: usize, fp: usize) -> Self {
        CallFrame { closure, ip, fp }
    }

    #[inline(always)]
    fn positions(&self) -> &[Position] {
        &self.closure.function().chunk().positions
    }

    #[inline(always)]
    fn code(&self) -> &[Instruction] {
        &self.closure.function().chunk().code
    }
}

pub struct Machine {
    // TODO: run a test for the dedicated current frame solution.
    frame: CallFrame,
    // FIXME: will it be faster to allocate CallFrame on stack?
    enclosing_frames: Vec<CallFrame>,

    arena: Arena,
    stack: Vec<Value>,
}

impl Machine {
    pub fn new(bundle: ClosureCompileBundle, mut arena: Arena) -> Self {
        let clock = NATIVECLOCK.with(|c| c.clone());
        let key = arena.alloc_string_ref(clock.name());
        arena.set_global(key, Value::NativeFn(clock));

        let init_closure = LoxClosure::new(bundle.function);
        Machine {
            frame: CallFrame::new(init_closure.clone(), 0, 0),
            enclosing_frames: Vec::with_capacity(64),
            arena,
            stack: vec![Value::Closure(init_closure)],
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
        let pos = self.frame.positions()[self.frame.ip - 1];
        Err(RuntimeError::new(pos, format(args)))
    }

    #[inline(always)]
    fn read_instr(&mut self) -> *const Instruction {
        let instr = &self.frame.code()[self.frame.ip] as *const Instruction;
        self.frame.ip += 1;
        instr
    }

    fn _debug_stack(&self) {
        if self.stack.is_empty() {
            println!("[]");
        } else {
            print!("[");
            for v in &self.stack {
                print!(" {}", v);
            }
            println!(" ]");
        }
    }

    fn call_value(&mut self, value: Value, arg_count: usize) -> VmResult<()> {
        match value {
            Value::Closure(closure) => self.call(closure, arg_count),
            Value::NativeFn(fun) => {
                self.check_arity(arg_count, fun.arity())?;
                let offset = self.stack.len() - arg_count;
                let result = fun.code()(&self.stack[offset..]);
                self.push(result);
                Ok(())
            }
            _ => {
                self.runtime_err(args!("Can only call functions and classes."))
            }
        }
    }

    fn check_arity(&self, arg_count: usize, arity: usize) -> VmResult<()> {
        if arg_count != arity {
            self.runtime_err(args!(
                "Expected {} arguments but got {}.",
                arity,
                arg_count
            ))
        } else {
            Ok(())
        }
    }

    fn call(&mut self, closure: LoxClosure, arg_count: usize) -> VmResult<()> {
        self.check_arity(arg_count, closure.function().arity())?;
        let mut frame =
            CallFrame::new(closure, 0, self.stack.len() - arg_count - 1);
        std::mem::swap(&mut self.frame, &mut frame);
        self.enclosing_frames.push(frame);
        Ok(())
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
            let instr = self.read_instr();
            // for debug
            // println!("{}", instr);
            // self._debug_stack();

            // Use unsafe to circumvent borrow checker, as we can tell that
            // the muttable parts (frame.ip, starck etc.) will not affect
            // the memory of Instruction.
            // TODO: Of course, we could design a struct RunningCallFrame<'a> to
            // eliminate the unsafe code
            match unsafe { &*instr } {
                Instruction::Return => {
                    let result = self.pop();
                    if self.enclosing_frames.is_empty() {
                        // return from __main__ entry function
                        self.pop();
                        return Ok(());
                    } else {
                        // drop local variables of callee function
                        self.stack.drain(self.frame.fp..);
                        self.push(result);

                        let mut frame = self.enclosing_frames.pop().unwrap();
                        std::mem::swap(&mut self.frame, &mut frame);
                    }
                }
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::Jump(offset) => {
                    self.frame.ip += offset;
                }
                Instruction::JumpIfFalse(offset) => {
                    if !self.peek(0).is_truthy() {
                        self.frame.ip += offset;
                    }
                }
                Instruction::Loop(offset) => {
                    self.frame.ip -= offset;
                }
                Instruction::Print => {
                    match self.pop() {
                        // debug
                        Value::Closure(c) => {
                            c.function().disassemble();
                        }
                        v => println!("{}", v),
                    }
                }
                Instruction::Call(arg_count) => {
                    let callee = self.peek(*arg_count).clone();
                    self.call_value(callee, *arg_count)?;
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
                    let val = self.stack[*idx + self.frame.fp].clone();
                    self.push(val);
                }
                Instruction::SetLocal(idx) => {
                    self.stack[*idx + self.frame.fp] = self.peek(0).clone();
                }
                Instruction::GetUpval(idx) => {}
                Instruction::SetUpval(idx) => {}
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

                Instruction::Closure(bundle) => {
                    let closure = LoxClosure::new(bundle.function.clone());
                    self.push(Value::Closure(closure))
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

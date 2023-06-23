use std::{cmp, io::Cursor};

use byteorder::{BigEndian, ReadBytesExt};

use crate::{code::opcodes, code::Instructions, compiler::Bytecode, object::Object};

const STACK_SIZE: usize = 2048;

struct Stack {
    stack: Vec<Object>,
    sp: usize,
}

impl Stack {
    fn new() -> Self {
        Self {
            stack: Vec::with_capacity(STACK_SIZE),
            sp: 0,
        }
    }

    fn push(&mut self, obj: &Object) -> Result<(), Error> {
        if self.sp >= STACK_SIZE {
            return Err(Error::StackOverflow);
        }
        let to_push = obj.clone();
        let slen = self.stack.len();
        match slen.cmp(&self.sp) {
            cmp::Ordering::Equal => {
                self.stack.push(to_push);
            }
            cmp::Ordering::Greater => {
                self.stack[self.sp] = to_push;
            }
            cmp::Ordering::Less => {
                // Tried pushing above the stack?? How
                return Err(Error::StackOverflow);
            }
        }
        self.sp += 1;
        Ok(())
    }

    fn pop(&mut self) -> Option<Object> {
        if self.sp > 0 {
            let top = self.stack_top().cloned();
            self.sp -= 1;
            top
        } else {
            None
        }
    }

    fn last_popped(&self) -> Option<&Object> {
        self.stack.get(self.sp)
    }

    fn stack_top(&self) -> Option<&Object> {
        match self.sp {
            0 => None,
            sp => Some(&self.stack[sp - 1]),
        }
    }
}

mod vm_state {
    pub struct Run;
    pub struct Init;
}

pub struct VM<State = vm_state::Init> {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Option<Stack>,
    #[allow(dead_code)]
    state: State,
}

#[derive(Debug)]
pub enum Error {
    StackOverflow,
    Bytecode(std::io::Error),
    UnknownOpcode(usize, u8),
}

impl VM {
    // Not ready for const fn yet
    #[allow(clippy::missing_const_for_fn)]
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: None,
            state: vm_state::Init,
        }
    }
}

impl<State> VM<State> {
    pub fn run(self) -> Result<VM<vm_state::Run>, Error> {
        let mut rdr = Cursor::new(&self.instructions);
        let mut stack = Stack::new();
        let mut ip = 0;
        loop {
            let opcode = match rdr.read_u8() {
                Err(ref e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
                Err(e) => return Err(Error::Bytecode(e)),
                Ok(opcode) => opcode,
            };
            let pc = ip;
            ip += 1;
            match opcode {
                opcodes::CONSTANT => {
                    let constant = rdr.read_u16::<BigEndian>().unwrap();
                    stack.push(&self.constants[constant as usize])?;
                    ip += 2;
                }
                opcodes::ADD | opcodes::SUB | opcodes::MUL | opcodes::DIV => {
                    let left = stack.pop();
                    let right = stack.pop();
                    match (left, right) {
                        (Some(Object::Integer(x)), Some(Object::Integer(y))) => {
                            let result = match opcode {
                                opcodes::ADD => x + y,
                                opcodes::SUB => y - x,
                                opcodes::MUL => x * y,
                                opcodes::DIV => y / x,
                                _ => panic!("invalid op"),
                            };
                            stack.push(&Object::Integer(result))?;
                        }
                        (Some(x), Some(y)) => panic!("invalid operation for {x} and {y}"),
                        (_, _) => panic!("invalid state"),
                    };
                }
                opcodes::POP => {
                    stack.pop();
                }
                op => return Err(Error::UnknownOpcode(pc, op)),
            }
        }
        Ok(VM::<vm_state::Run> {
            constants: self.constants,
            instructions: self.instructions,
            stack: Some(stack),
            state: vm_state::Run,
        })
    }
}

impl VM<vm_state::Run> {
    pub fn last_popped(&self) -> Option<&Object> {
        self.stack.as_ref().and_then(Stack::last_popped)
    }
}

#[cfg(test)]
mod tests {
    use crate::util::test_utils::{compile_program, test_object, Constant};

    use super::VM;

    type Test<'a> = (&'a str, Constant);

    #[test]
    pub fn test_integer_arithmetic() {
        run_vm_tests(vec![
            ("1", Constant::Int(1)),
            ("2", Constant::Int(2)),
            ("1 + 2", Constant::Int(3)),
            ("1 - 2", Constant::Int(-1)),
            ("1 * 2", Constant::Int(2)),
            ("4 / 2", Constant::Int(2)),
            ("50 / 2 * 2 + 10 - 5", Constant::Int(55)),
            ("5 + 5 + 5 + 5 - 10", Constant::Int(10)),
            ("2 * 2 * 2 * 2 * 2", Constant::Int(32)),
            ("5 * 2 + 10", Constant::Int(20)),
            ("5 + 2 * 10", Constant::Int(25)),
            ("5 * (2 + 10)", Constant::Int(60)),
        ])
    }

    fn run_vm_tests(tests: Vec<Test>) {
        for (input, expected) in tests {
            let bytecode = compile_program(input);
            let vm = match VM::new(bytecode).run() {
                Ok(vm) => vm,
                Err(err) => panic!("vm error: {err:?}"),
            };
            let got = vm.last_popped().expect("stack value");
            if let Err(err) = test_object(got, &expected) {
                panic!("failed test for input {input}. {err}")
            }
        }
    }
}

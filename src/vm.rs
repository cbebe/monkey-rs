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

    pub fn stack_top(&self) -> Option<&Object> {
        match self.sp {
            0 => None,
            sp => Some(&self.stack[sp - 1]),
        }
    }
}

struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Option<Stack>,
}

#[derive(Debug)]
enum Error {
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
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
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
                op => return Err(Error::UnknownOpcode(pc, op)),
            }
        }
        self.stack = Some(stack);
        Ok(())
    }

    pub fn stack_top(&self) -> Option<&Object> {
        self.stack.as_ref().and_then(Stack::stack_top)
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Node, compiler::Compiler, object::Object, parser};

    use super::VM;

    enum Constant {
        Int(i64),
    }

    type Test<'a> = (&'a str, Constant);

    #[test]
    pub fn test_integer_arithmetic() {
        run_vm_tests(vec![
            ("1", Constant::Int(1)),
            ("2", Constant::Int(2)),
            ("1 + 2", Constant::Int(2)), // TODO: Fixme
        ])
    }

    fn run_vm_tests(tests: Vec<Test>) {
        for (input, expected) in tests {
            let program = match parser::program(input) {
                Ok(p) => p.1,
                Err(e) => panic!("invalid program {e}"),
            };

            let mut compiler = Compiler::new();
            if let Err(err) = compiler.compile(Node::Program(program)) {
                panic!("compiler error: {err:?}");
            };
            let mut vm = VM::new(compiler.bytecode());
            if let Err(err) = vm.run() {
                panic!("vm error: {err:?}");
            };
            let got = vm.stack_top().expect("stack value");
            test_object(got, &expected);
        }
    }

    fn test_object(got: &Object, want: &Constant) {
        assert!(match (want, got) {
            (Constant::Int(x), Object::Integer(y)) if x == y => true,
            _ => false,
        });
    }
}

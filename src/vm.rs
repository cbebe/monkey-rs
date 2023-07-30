use std::{cmp, io::Cursor};

use byteorder::{BigEndian, ReadBytesExt};

use crate::{code::opcodes, code::Instructions, compiler::Bytecode, object::Object};

const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

struct Stack {
    stack: Vec<Object>,
    sp: usize,
}

impl Default for Stack {
    fn default() -> Self {
        Self::new()
    }
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

    fn try_pop(&mut self) -> Result<Object, Error> {
        self.pop().ok_or(Error::EmptyStack)
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
    #[allow(dead_code)]
    pub globals: Option<Vec<Object>>,
}

#[derive(Debug)]
pub enum Error {
    StackOverflow,
    TooManyGlobals,
    EmptyStack,
    Bytecode(std::io::Error),
    UnknownOpcode(usize, u8),
    InvalidOp(&'static str, u8),
    InvalidBinary(Object, Object),
    InvalidUnary(Object),
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
            globals: None,
        }
    }

    pub fn with_globals(self, globals: Vec<Object>) -> Self {
        Self {
            globals: Some(globals),
            constants: self.constants,
            stack: self.stack,
            state: self.state,
            instructions: self.instructions,
        }
    }
}

impl<State> VM<State> {
    pub fn run(self) -> Result<VM<vm_state::Run>, Error> {
        let mut rdr = Cursor::new(&self.instructions);
        let mut stack = self.stack.unwrap_or_default();
        let mut globals = self.globals.unwrap_or_default();
        let mut ip: usize = 0;
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
                    ip += 2;
                    stack.push(&self.constants[constant as usize])?;
                }
                opcodes::TRUE => stack.push(&TRUE)?,
                opcodes::FALSE => stack.push(&FALSE)?,
                opcodes::EQUAL | opcodes::NOT_EQUAL | opcodes::GREATER_THAN => {
                    let result = Self::exec_cmp(opcode, &mut stack)?;
                    stack.push(&Object::Boolean(result))?;
                }
                opcodes::ADD | opcodes::SUB | opcodes::MUL | opcodes::DIV => {
                    let result = Self::exec_bin_op(opcode, &mut stack)?;
                    stack.push(&result)?;
                }
                opcodes::BANG => {
                    let result = Self::exec_bang_op(&mut stack)?;
                    stack.push(&Object::Boolean(result))?;
                }
                opcodes::MINUS => {
                    let result = Self::exec_minus_op(&mut stack)?;
                    stack.push(&Object::Integer(result))?;
                }
                opcodes::POP => {
                    stack.try_pop()?;
                }
                opcodes::JUMP => {
                    ip = rdr.read_u16::<BigEndian>().unwrap().into();
                    rdr.set_position(ip.try_into().unwrap());
                }
                opcodes::JUMP_NOT_TRUTHY => {
                    let pos: usize = rdr.read_u16::<BigEndian>().unwrap().into();
                    let condition = stack.try_pop()?;
                    ip += 2;
                    if !Self::is_truthy(condition) {
                        ip = pos;
                        rdr.set_position(ip.try_into().unwrap());
                    }
                }
                opcodes::NULL => stack.push(&NULL)?,
                opcodes::GET_GLOBAL => {
                    let global_index: usize = rdr.read_u16::<BigEndian>().unwrap().into();
                    ip += 2;
                    stack.push(&globals[global_index])?;
                }
                opcodes::SET_GLOBAL => {
                    let global_index: usize = rdr.read_u16::<BigEndian>().unwrap().into();
                    ip += 2;
                    let to_set = stack.try_pop()?;
                    let slen = globals.len();
                    match slen.cmp(&global_index) {
                        cmp::Ordering::Equal => {
                            globals.push(to_set);
                        }
                        cmp::Ordering::Greater => {
                            globals[global_index] = to_set;
                        }
                        cmp::Ordering::Less => {
                            // Tried pushing above the stack?? How
                            return Err(Error::TooManyGlobals);
                        }
                    }
                }
                op => return Err(Error::UnknownOpcode(pc, op)),
            }
        }
        Ok(VM::<vm_state::Run> {
            constants: self.constants,
            instructions: self.instructions,
            stack: Some(stack),
            state: vm_state::Run,
            globals: Some(globals),
        })
    }

    fn exec_cmp(opcode: u8, stack: &mut Stack) -> Result<bool, Error> {
        let right = stack.try_pop()?;
        let left = stack.try_pop()?;
        match (left, right) {
            (Object::Boolean(x), Object::Boolean(y)) => match opcode {
                opcodes::EQUAL => Ok(x == y),
                opcodes::NOT_EQUAL => Ok(x != y),
                op => Err(Error::InvalidOp("bool", op)),
            },
            (Object::Integer(x), Object::Integer(y)) => match opcode {
                opcodes::EQUAL => Ok(x == y),
                opcodes::NOT_EQUAL => Ok(x != y),
                opcodes::GREATER_THAN => Ok(x > y),
                op => Err(Error::InvalidOp("int", op)),
            },
            (x, y) => Err(Error::InvalidBinary(x, y)),
        }
    }

    fn exec_minus_op(stack: &mut Stack) -> Result<i64, Error> {
        match stack.try_pop()? {
            Object::Integer(x) => Ok(-x),
            x => Err(Error::InvalidUnary(x)),
        }
    }

    fn is_truthy(obj: Object) -> bool {
        match obj {
            Object::Boolean(b) => b,
            // Also consider null, 0, and empty string as falsy
            Object::Null | Object::Integer(0) => false,
            Object::String(x) if x.is_empty() => false,
            _ => true,
        }
    }

    fn exec_bang_op(stack: &mut Stack) -> Result<bool, Error> {
        let value = Self::is_truthy(stack.try_pop()?);
        Ok(!value)
    }

    fn exec_bin_op(opcode: u8, stack: &mut Stack) -> Result<Object, Error> {
        let left = stack.try_pop()?;
        let right = stack.try_pop()?;
        match (left, right) {
            (Object::Integer(x), Object::Integer(y)) => match opcode {
                opcodes::ADD => Ok(Object::Integer(x + y)),
                opcodes::SUB => Ok(Object::Integer(y - x)),
                opcodes::MUL => Ok(Object::Integer(x * y)),
                opcodes::DIV => Ok(Object::Integer(y / x)),
                _ => Err(Error::InvalidOp("int", opcode)),
            },
            (Object::String(x), Object::String(y)) => match opcode {
                opcodes::ADD => Ok(Object::String(y + &x)),
                _ => Err(Error::InvalidOp("string", opcode)),
            },
            (x, y) => Err(Error::InvalidBinary(x, y)),
        }
    }
}

impl VM<vm_state::Run> {
    pub fn last_popped(&self) -> Option<&Object> {
        self.stack.as_ref().and_then(Stack::last_popped)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        code::Disassembled,
        util::test_utils::{compile_program, test_object, Constant},
    };

    use super::VM;

    type Test<'a> = (&'a str, Constant);

    #[test]
    fn test_integer_arithmetic() {
        use Constant::Int;
        run_vm_tests(vec![
            ("1", Int(1)),
            ("2", Int(2)),
            ("1 + 2", Int(3)),
            ("1 - 2", Int(-1)),
            ("1 * 2", Int(2)),
            ("4 / 2", Int(2)),
            ("50 / 2 * 2 + 10 - 5", Int(55)),
            ("5 + 5 + 5 + 5 - 10", Int(10)),
            ("2 * 2 * 2 * 2 * 2", Int(32)),
            ("5 * 2 + 10", Int(20)),
            ("5 + 2 * 10", Int(25)),
            ("5 * (2 + 10)", Int(60)),
            ("-5", Int(-5)),
            ("-10", Int(-10)),
            ("-50 + 100 + -50", Int(0)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Int(50)),
        ]);
    }

    #[test]
    fn test_boolean_expressions() {
        use Constant::Bool;
        run_vm_tests(vec![
            ("true", Bool(true)),
            ("false", Bool(false)),
            ("1 < 2", Bool(true)),
            ("1 > 2", Bool(false)),
            ("1 < 1", Bool(false)),
            ("1 > 1", Bool(false)),
            ("1 == 1", Bool(true)),
            ("1 != 1", Bool(false)),
            ("1 == 2", Bool(false)),
            ("1 != 2", Bool(true)),
            ("true == true", Bool(true)),
            ("false == false", Bool(true)),
            ("true == false", Bool(false)),
            ("true != false", Bool(true)),
            ("false != true", Bool(true)),
            ("(1 < 2) == true", Bool(true)),
            ("(1 < 2) == false", Bool(false)),
            ("(1 > 2) == true", Bool(false)),
            ("(1 > 2) == false", Bool(true)),
            ("!true", Bool(false)),
            ("!false", Bool(true)),
            ("!5", Bool(false)),
            ("!!true", Bool(true)),
            ("!!false", Bool(false)),
            ("!!5", Bool(true)),
            ("!(if (false) { 5; })", Bool(true)),
        ]);
    }

    #[test]
    fn test_conditionals() {
        use Constant::{Int, Null};
        run_vm_tests(vec![
            ("if (true) { 10 }", Int(10)),
            ("if (1) { 10 }", Int(10)),
            ("if (1 < 2) { 10 }", Int(10)),
            ("if (true) { 10 } else { 20 }", Int(10)),
            ("if (false) { 10 } else { 20 } ", Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Int(10)),
            ("if (1 > 2) { 10 } else { 20 }", Int(20)),
            ("if (1 > 2) { 10 }", Null),
            ("if (false) { 10 }", Null),
            ("if ((if (false) { 10 })) { 10 } else { 20 }", Int(20)),
        ]);
    }

    #[test]
    fn test_global_test_statements() {
        use Constant::Int;
        run_vm_tests(vec![
            ("let one = 1; one", Int(1)),
            ("let one = 1; let two = 2; one + two", Int(3)),
            ("let one = 1; let two = one + one; one + two", Int(3)),
        ]);
    }

    #[test]
    fn test_string_expressions() {
        use Constant::String;
        run_vm_tests(vec![
            (r#""monkey""#, String(Rc::from("monkey"))),
            (r#""mon" + "key""#, String(Rc::from("monkey"))),
            (
                r#""mon" + "key" + "banana""#,
                String(Rc::from("monkeybanana")),
            ),
        ]);
    }

    fn run_vm_tests(tests: Vec<Test>) {
        for (input, expected) in tests {
            let bytecode = compile_program(input);
            let disassembly = Disassembled(bytecode.instructions.clone());
            let vm = match VM::new(bytecode).run() {
                Ok(vm) => vm,
                Err(err) => panic!("vm error: {err:?}\ninput: {input}\n{disassembly}"),
            };
            let got = vm.last_popped().expect("stack value");
            if let Err(err) = test_object(got, &expected) {
                panic!("failed test\ninput: {input}\n{err}")
            }
        }
    }
}

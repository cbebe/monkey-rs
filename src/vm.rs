use std::{cmp, collections::BTreeMap, io::Cursor};

use byteorder::{BigEndian, ReadBytesExt};

use crate::{
    code::{opcodes, Instructions},
    object::{HashKey, HashPair, Hashable, Object},
};

const STACK_SIZE: usize = 2048;
const MAX_FRAMES: usize = 1024;
pub const GLOBALS_SIZE: usize = 65536;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

struct Frame {
    pub func: Instructions,
    pub ip: usize,
}

impl Frame {
    pub fn new(func: Instructions) -> Self {
        Self { func, ip: 0 }
    }
}

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
            cmp::Ordering::Equal => self.stack.push(to_push),
            cmp::Ordering::Greater => self.stack[self.sp] = to_push,
            // Tried pushing above the stack?? How
            cmp::Ordering::Less => return Err(Error::StackOverflow),
        };
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
    frames: Option<Vec<Frame>>,
    stack: Option<Stack>,
    state: State,
    pub globals: Option<Vec<Object>>,
}

#[derive(Debug)]
pub enum Error {
    StackOverflow,
    OutOfFrames,
    TooManyGlobals,
    EmptyStack,
    Bytecode(std::io::Error),
    UnknownOpcode(usize, u8),
    InvalidOp(&'static str, u8),
    InvalidBinary(Object, Object),
    InvalidUnary(Object),
    InvalidIndex(Object, Object),
    UnhashableType(Object),
    KeyAlreadyExists(Object),
    CallNonFunction(Object),
}

impl VM {
    #[allow(clippy::missing_const_for_fn)]
    pub fn new(bytecode: crate::compiler::Bytecode) -> Self {
        let main_frame = Frame::new(bytecode.instructions);
        let mut frames = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);
        Self {
            constants: bytecode.constants,
            frames: Some(frames),
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
            frames: self.frames,
        }
    }
}

enum Execute {
    Done,
    Continue,
}

impl<State> VM<State> {
    pub fn run(self) -> Result<VM<vm_state::Run>, Error> {
        let mut frames = self.frames.unwrap_or_default();
        let mut stack = self.stack.unwrap_or_default();
        let mut globals = self.globals.unwrap_or_default();
        let constants = self.constants;
        while {
            let frame = frames.last().ok_or(Error::OutOfFrames)?;
            frame.ip < frame.func.len()
        } {
            if matches!(
                Self::execute(&mut frames, &mut stack, &mut globals, &constants)?,
                Execute::Done
            ) {
                break;
            }
        }

        Ok(VM::<vm_state::Run> {
            constants,
            frames: Some(frames),
            stack: Some(stack),
            state: vm_state::Run,
            globals: Some(globals),
        })
    }

    fn execute(
        frames: &mut Vec<Frame>,
        stack: &mut Stack,
        globals: &mut Vec<Object>,
        constants: &[Object],
    ) -> Result<Execute, Error> {
        let frame = frames.last_mut().ok_or(Error::OutOfFrames)?;
        let mut rdr = Cursor::new(&frame.func);
        rdr.set_position(frame.ip.try_into().unwrap());
        let opcode = match rdr.read_u8() {
            Err(ref e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                return Ok(Execute::Done)
            }
            Err(e) => return Err(Error::Bytecode(e)),
            Ok(opcode) => opcode,
        };
        let pc = frame.ip;
        frame.ip += 1;
        match opcode {
            opcodes::CONSTANT => {
                let constant = rdr.read_u16::<BigEndian>().unwrap();
                frame.ip += 2;
                stack.push(&constants[constant as usize])?;
            }
            opcodes::TRUE => stack.push(&TRUE)?,
            opcodes::FALSE => stack.push(&FALSE)?,
            opcodes::EQUAL | opcodes::NOT_EQUAL | opcodes::GREATER_THAN => {
                let result = Self::exec_cmp(opcode, stack)?;
                stack.push(&Object::Boolean(result))?;
            }
            opcodes::ADD | opcodes::SUB | opcodes::MUL | opcodes::DIV => {
                let result = Self::exec_bin_op(opcode, stack)?;
                stack.push(&result)?;
            }
            opcodes::BANG => {
                let result = Self::exec_bang_op(stack)?;
                stack.push(&Object::Boolean(result))?;
            }
            opcodes::MINUS => {
                let result = Self::exec_minus_op(stack)?;
                stack.push(&Object::Integer(result))?;
            }
            opcodes::POP => {
                stack.try_pop()?;
            }
            opcodes::JUMP => {
                frame.ip = rdr.read_u16::<BigEndian>().unwrap().into();
                rdr.set_position(frame.ip.try_into().unwrap());
            }
            opcodes::JUMP_NOT_TRUTHY => {
                let pos: usize = rdr.read_u16::<BigEndian>().unwrap().into();
                let condition = stack.try_pop()?;
                frame.ip += 2;
                if !Self::is_truthy(condition) {
                    frame.ip = pos;
                    rdr.set_position(frame.ip.try_into().unwrap());
                }
            }
            opcodes::NULL => stack.push(&NULL)?,
            opcodes::GET_GLOBAL | opcodes::SET_GLOBAL => {
                let global_index: usize = rdr.read_u16::<BigEndian>().unwrap().into();
                frame.ip += 2;
                Self::global(opcode, global_index, globals, stack)?;
            }
            opcodes::ARRAY => {
                let num_elems: usize = rdr.read_u16::<BigEndian>().unwrap().into();
                frame.ip += 2;
                let arr = Self::build_array(stack, num_elems)?;
                stack.push(&arr)?;
            }
            opcodes::HASH => {
                let num_elems: usize = rdr.read_u16::<BigEndian>().unwrap().into();
                frame.ip += 2;
                let hash = Self::build_hash(stack, num_elems)?;
                stack.push(&hash)?;
            }
            opcodes::INDEX => Self::exec_index(stack)?,
            opcodes::CALL => {
                let obj = stack.stack_top().ok_or(Error::EmptyStack)?;
                if let Object::Function(func) = obj {
                    let f = Frame::new(func.clone());
                    frames.push(f);
                } else {
                    return Err(Error::CallNonFunction(obj.clone()));
                }
            }
            opcodes::RETURN_VALUE => {
                let ret = stack.try_pop()?;
                frames.pop();
                stack.try_pop()?;
                stack.push(&ret)?;
            }
            opcodes::RETURN => {
                frames.pop();
                stack.try_pop()?;
                stack.push(&NULL)?;
            }
            op => return Err(Error::UnknownOpcode(pc, op)),
        }
        Ok(Execute::Continue)
    }

    fn global(
        opcode: u8,
        global_index: usize,
        globals: &mut Vec<Object>,
        stack: &mut Stack,
    ) -> Result<(), Error> {
        match opcode {
            opcodes::GET_GLOBAL => {
                stack.push(&globals[global_index])?;
            }
            opcodes::SET_GLOBAL => {
                let to_set = stack.try_pop()?;
                let slen = globals.len();
                match slen.cmp(&global_index) {
                    cmp::Ordering::Equal => globals.push(to_set),
                    cmp::Ordering::Greater => globals[global_index] = to_set,
                    cmp::Ordering::Less => return Err(Error::TooManyGlobals),
                };
            }
            op => return Err(Error::InvalidOp("global", op)),
        }
        Ok(())
    }

    fn build_array(stack: &mut Stack, num_elems: usize) -> Result<Object, Error> {
        // Might be bad for perf but whatevs
        let mut arr = Vec::<Object>::with_capacity(num_elems);
        for _ in 0..num_elems {
            arr.push(stack.try_pop()?);
        }
        arr.reverse();
        Ok(Object::Array(arr))
    }

    fn build_hash(stack: &mut Stack, num_elems: usize) -> Result<Object, Error> {
        let mut hash = BTreeMap::new();
        for _ in (0..num_elems).step_by(2) {
            let value = stack.try_pop()?;
            let key = stack.try_pop()?;
            let hash_key = Self::get_hash_key(&key)?;
            if let Some(existing) = hash.insert(hash_key, HashPair { key, value }) {
                return Err(Error::KeyAlreadyExists(existing.key));
            }
        }

        Ok(Object::Hash(hash))
    }

    fn exec_index(stack: &mut Stack) -> Result<(), Error> {
        let index = stack.try_pop()?;
        let left = stack.try_pop()?;
        match (left, &index) {
            (Object::Array(x), Object::Integer(i)) => {
                let max = x.len() as i64 - 1;
                stack.push(if *i < 0 || *i > max {
                    &Object::Null
                } else {
                    &x[*i as usize]
                })?;
            }
            (Object::Hash(x), Object::Integer(_) | Object::Boolean(_) | Object::String(_)) => {
                let key = Self::get_hash_key(&index)?;
                stack.push(
                    x.get(&key)
                        .map_or_else(|| &Object::Null, |item| &item.value),
                )?;
            }
            (x, i) => return Err(Error::InvalidIndex(x, i.clone())),
        };

        Ok(())
    }

    fn get_hash_key(obj: &Object) -> Result<HashKey, Error> {
        match obj {
            Object::Integer(x) => Ok(x.hash_key()),
            Object::Boolean(x) => Ok(x.hash_key()),
            Object::String(x) => Ok(x.hash_key()),
            Object::Null | Object::Array(_) | Object::Hash(_) | Object::Function(_) => {
                Err(Error::UnhashableType(obj.clone()))
            }
        }
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

    use crate::util::test_utils::{self, Constant};

    use super::VM;

    // https://stackoverflow.com/a/27582993
    macro_rules! collection {
        // map-like
        ($($k:expr => $v:expr),* $(,)?) => {{
            core::convert::From::from([$(($k, $v),)*])
        }};
        // set-like
        ($($v:expr),* $(,)?) => {{
            core::convert::From::from([$($v,)*])
        }};
    }

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

    #[test]
    fn test_array_literals() {
        use Constant::{Array, Int};
        run_vm_tests(vec![
            ("[]", Array(vec![])),
            ("[1, 2, 3]", Array(vec![Int(1), Int(2), Int(3)])),
            (
                "[1 + 2, 3 * 4, 5 + 6]",
                Array(vec![Int(3), Int(12), Int(11)]),
            ),
        ]);
    }

    #[test]
    fn test_hash_literals() {
        use Constant::{Hash, Int};
        run_vm_tests(vec![
            ("{}", Hash(collection! {})),
            (
                "{1: 2, 2: 3}",
                Hash(collection! { Int(1) => Int(2), Int(2) => Int(3) }),
            ),
            (
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                Hash(collection! { Int(2) => Int(4), Int(6) => Int(16) }),
            ),
        ]);
    }

    #[test]
    fn test_index_expressions() {
        use Constant::{Int, Null};
        run_vm_tests(vec![
            ("[1, 2, 3][1]", Int(2)),
            ("[1, 2, 3][0 + 2]", Int(3)),
            ("[[1, 1, 1]][0][0]", Int(1)),
            ("[][0]", Null),
            ("[1, 2, 3][99]", Null),
            ("[1][-1]", Null),
            ("{1: 1, 2: 2}[1]", Int(1)),
            ("{1: 1, 2: 2}[2]", Int(2)),
            ("{1: 1}[0]", Null),
            ("{}[0]", Null),
        ]);
    }

    #[test]
    fn test_calling_functions() {
        use Constant::Int;
        run_vm_tests(vec![
            ("let fivePlusTen = fn() { 5 + 10 }; fivePlusTen();", Int(15)),
            (
                "let one = fn() { 1; }; let two = fn() { 2; }; one() + two();",
                Int(3),
            ),
            (
                "let a = fn() { 1 }; let b = fn() { a() + 1 }; let c = fn() { b() + 1 }; c();",
                Int(3),
            ),
        ]);
    }

    #[test]
    fn test_functions_with_return_statement() {
        use Constant::Int;
        run_vm_tests(vec![
            (
                "let earlyExit = fn() { return 99; 100; }; earlyExit();",
                Int(99),
            ),
            (
                "let earlyExit = fn() { return 99; return 100; }; earlyExit();",
                Int(99),
            ),
        ]);
    }

    #[test]
    fn test_functions_without_return_value() {
        use Constant::Null;
        run_vm_tests(vec![
            ("let noReturn = fn() { }; noReturn();", Null),
            ("let noReturn = fn() { }; let noReturnTwo = fn() { noReturn(); }; noReturn(); noReturnTwo();", Null),
        ]);
    }

    #[test]
    fn test_first_class_functions() {
        use Constant::Int;
        run_vm_tests(vec![(
            "let returnsOne = fn() { 1; }; let returnsOneReturner = fn() { returnsOne; }; returnsOneReturner()();",
            Int(1),
        )]);
    }

    fn run_vm_tests(tests: Vec<Test>) {
        for (input, expected) in tests {
            let bytecode = test_utils::compile_program(input);
            let disassembly = crate::code::Disassembled(bytecode.instructions.clone());
            let vm = match VM::new(bytecode).run() {
                Ok(vm) => vm,
                Err(err) => panic!("vm error: {err:?}\ninput: {input}\n{disassembly}"),
            };
            let got = vm
                .last_popped()
                .unwrap_or_else(|| panic!("no stack value\ninput: {input}\n{disassembly}"));
            if let Err(err) = test_utils::test_object(got, &expected) {
                panic!("failed test\ninput: {input}\n{err}")
            }
        }
    }
}

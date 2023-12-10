use crate::{
    code::{
        opcodes::{
            ADD, ARRAY, BANG, CALL, CONSTANT, DIV, EQUAL, FALSE, GET_GLOBAL, GET_LOCAL,
            GREATER_THAN, HASH, INDEX, JUMP, JUMP_NOT_TRUTHY, MINUS, MUL, NOT_EQUAL, NULL, POP,
            RETURN, RETURN_VALUE, SET_GLOBAL, SET_LOCAL, SUB, TRUE,
        },
        Instructions,
    },
    object::{HashKey, HashPair, Hashable, Object},
};
use byteorder::{BigEndian, ReadBytesExt};
use std::{cmp, collections::BTreeMap, io::Cursor};

const STACK_SIZE: usize = 2048;
const MAX_FRAMES: usize = 1024;
pub const GLOBALS_SIZE: usize = 65536;

const TRUE_OBJ: Object = Object::Boolean(true);
const FALSE_OBJ: Object = Object::Boolean(false);
const NULL_OBJ: Object = Object::Null;

struct Frame {
    pub func: Instructions,
    pub ip: usize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(func: Instructions, base_pointer: usize) -> Self {
        Self {
            func,
            ip: 0,
            base_pointer,
        }
    }
}

struct Stack {
    stack: [Object; STACK_SIZE],
    sp: usize,
}

impl Default for Stack {
    fn default() -> Self {
        Self::new()
    }
}

impl Stack {
    const fn new() -> Self {
        Self {
            stack: [NULL_OBJ; STACK_SIZE],
            sp: 0,
        }
    }

    fn push(&mut self, obj: &Object) -> Result<(), Error> {
        if self.sp >= STACK_SIZE {
            return Err(Error::StackOverflow);
        }
        let to_push = obj.clone();
        self.stack[self.sp] = to_push;
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

    const fn stack_top(&self) -> Option<&Object> {
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
    InvalidAddress(usize),
    UnhashableType(Object),
    KeyAlreadyExists(Object),
    CallNonFunction(Object),
}

impl VM {
    #[allow(clippy::missing_const_for_fn)]
    pub fn new(bytecode: crate::compiler::Bytecode) -> Self {
        let main_frame = Frame::new(bytecode.instructions, 0);
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

    fn push_value(op: u8, stack: &mut Stack) -> Result<(), Error> {
        let result = match op {
            EQUAL | NOT_EQUAL | GREATER_THAN => Self::exec_cmp(op, stack)?,
            ADD | SUB | MUL | DIV => Self::exec_bin_op(op, stack)?,
            BANG => Self::exec_bang_op(stack)?,
            MINUS => Self::exec_minus_op(stack)?,
            INDEX => Self::exec_index(stack)?,
            NULL => NULL_OBJ,
            TRUE => TRUE_OBJ,
            FALSE => FALSE_OBJ,
            op => return Err(Error::InvalidOp("push_value", op)),
        };
        stack.push(&result)
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

        macro_rules! read_word {
            () => {
                read_word!(word)
            };
            ($name: ident) => {{
                let $name: usize = rdr.read_u16::<BigEndian>().unwrap().into();
                frame.ip += 2;
                $name
            }};
        }

        macro_rules! read_byte {
            () => {
                read_byte!(byte)
            };
            ($name: ident) => {{
                let $name: usize = rdr.read_u8().unwrap().into();
                frame.ip += 1;
                $name
            }};
        }

        match opcode {
            CONSTANT => {
                stack.push(&constants[read_word!(constant)])?;
            }
            POP => {
                stack.try_pop()?;
            }
            TRUE | FALSE | EQUAL | NOT_EQUAL | GREATER_THAN | ADD | SUB | MUL | DIV | BANG
            | MINUS | NULL | INDEX => {
                Self::push_value(opcode, stack)?;
            }
            JUMP => {
                frame.ip = read_word!(address);
                frame
                    .ip
                    .try_into()
                    .map(|ip| rdr.set_position(ip))
                    .or(Err(Error::InvalidAddress(frame.ip)))?;
            }
            JUMP_NOT_TRUTHY => {
                let pos = read_word!(pos);
                let condition = stack.try_pop()?;
                if !Self::is_truthy(condition) {
                    frame.ip = pos;
                    rdr.set_position(frame.ip.try_into().unwrap());
                }
            }
            GET_GLOBAL | SET_GLOBAL => {
                Self::global(opcode, read_word!(global_index), globals, stack)?;
            }
            SET_LOCAL | GET_LOCAL => {
                Self::local(opcode, read_byte!(local_index), frame.base_pointer, stack)?;
            }
            ARRAY => {
                let arr = Self::build_array(stack, read_word!(num_elems))?;
                stack.push(&arr)?;
            }
            HASH => {
                let hash = Self::build_hash(stack, read_word!(num_elems))?;
                stack.push(&hash)?;
            }
            CALL => {
                Self::call_function(read_byte!(args), stack, frames)?;
            }
            RETURN_VALUE | RETURN => {
                let ret = if opcode == RETURN_VALUE {
                    stack.try_pop()?
                } else {
                    NULL_OBJ
                };
                stack.sp = frames.pop().ok_or(Error::OutOfFrames)?.base_pointer - 1;
                stack.push(&ret)?;
            }
            op => return Err(Error::UnknownOpcode(pc, op)),
        }
        Ok(Execute::Continue)
    }

    fn call_function(
        num_args: usize,
        stack: &mut Stack,
        frames: &mut Vec<Frame>,
    ) -> Result<(), Error> {
        let fn_idx = stack.sp - 1 - num_args;
        let obj = &stack.stack[fn_idx];
        if let Object::Function {
            instructions: func,
            num_locals,
        } = obj
        {
            let frame = Frame::new(func.clone(), stack.sp - num_args);
            let base_pointer = frame.base_pointer;
            frames.push(frame);
            let to_add: usize = (*num_locals).into();
            stack.sp = base_pointer + to_add;
        } else {
            return Err(Error::CallNonFunction(obj.clone()));
        }
        Ok(())
    }

    fn global(
        opcode: u8,
        global_index: usize,
        globals: &mut Vec<Object>,
        stack: &mut Stack,
    ) -> Result<(), Error> {
        match opcode {
            GET_GLOBAL => {
                stack.push(&globals[global_index])?;
            }
            SET_GLOBAL => {
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

    fn local(
        opcode: u8,
        local_index: usize,
        base_pointer: usize,
        stack: &mut Stack,
    ) -> Result<(), Error> {
        match opcode {
            GET_LOCAL => {
                let item = stack.stack[base_pointer + local_index].clone();
                stack.push(&item)?;
            }
            SET_LOCAL => {
                let to_set = stack.try_pop()?;
                stack.stack[base_pointer + local_index] = to_set;
            }
            op => return Err(Error::InvalidOp("local", op)),
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

    fn exec_index(stack: &mut Stack) -> Result<Object, Error> {
        let index = stack.try_pop()?;
        let left = stack.try_pop()?;
        Ok(match (left, &index) {
            (Object::Array(x), Object::Integer(i)) => {
                let max = x.len() as i64 - 1;
                if *i < 0 || *i > max {
                    Object::Null
                } else {
                    // It was originally a usize anyway
                    #[allow(clippy::cast_sign_loss)]
                    x[*i as usize].clone()
                }
            }
            (Object::Hash(x), Object::Integer(_) | Object::Boolean(_) | Object::String(_)) => {
                let key = Self::get_hash_key(&index)?;
                x.get(&key)
                    .map_or_else(|| &Object::Null, |item| &item.value)
                    .clone()
            }
            (x, i) => return Err(Error::InvalidIndex(x, i.clone())),
        })
    }

    fn get_hash_key(obj: &Object) -> Result<HashKey, Error> {
        match obj {
            Object::Integer(x) => Ok(x.hash_key()),
            Object::Boolean(x) => Ok(x.hash_key()),
            Object::String(x) => Ok(x.hash_key()),
            Object::Null | Object::Array(_) | Object::Hash(_) | Object::Function { .. } => {
                Err(Error::UnhashableType(obj.clone()))
            }
        }
    }

    fn exec_cmp(opcode: u8, stack: &mut Stack) -> Result<Object, Error> {
        let right = stack.try_pop()?;
        let left = stack.try_pop()?;
        let res = match (left, right) {
            (Object::Boolean(x), Object::Boolean(y)) => match opcode {
                EQUAL => Ok(x == y),
                NOT_EQUAL => Ok(x != y),
                op => Err(Error::InvalidOp("bool", op)),
            },
            (Object::Integer(x), Object::Integer(y)) => match opcode {
                EQUAL => Ok(x == y),
                NOT_EQUAL => Ok(x != y),
                GREATER_THAN => Ok(x > y),
                op => Err(Error::InvalidOp("int", op)),
            },
            (x, y) => Err(Error::InvalidBinary(x, y)),
        }?;
        Ok(Object::Boolean(res))
    }

    fn exec_minus_op(stack: &mut Stack) -> Result<Object, Error> {
        Ok(Object::Integer(match stack.try_pop()? {
            Object::Integer(x) => Ok(-x),
            x => Err(Error::InvalidUnary(x)),
        }?))
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

    fn exec_bang_op(stack: &mut Stack) -> Result<Object, Error> {
        let value = Self::is_truthy(stack.try_pop()?);
        Ok(Object::Boolean(!value))
    }

    fn exec_bin_op(opcode: u8, stack: &mut Stack) -> Result<Object, Error> {
        let left = stack.try_pop()?;
        let right = stack.try_pop()?;
        match (left, right) {
            (Object::Integer(x), Object::Integer(y)) => match opcode {
                ADD => Ok(Object::Integer(x + y)),
                SUB => Ok(Object::Integer(y - x)),
                MUL => Ok(Object::Integer(x * y)),
                DIV => Ok(Object::Integer(y / x)),
                _ => Err(Error::InvalidOp("int", opcode)),
            },
            (Object::String(x), Object::String(y)) => match opcode {
                ADD => Ok(Object::String(y + &x)),
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

use std::io::Cursor;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

pub type Instructions = Vec<u8>;

pub mod opcodes {
    pub const CONSTANT: u8 = 0;
    pub const ADD: u8 = 1;
    pub const POP: u8 = 2;
    pub const SUB: u8 = 3;
    pub const MUL: u8 = 4;
    pub const DIV: u8 = 5;
    pub const TRUE: u8 = 6;
    pub const FALSE: u8 = 7;
    pub const EQUAL: u8 = 8;
    pub const NOT_EQUAL: u8 = 9;
    pub const GREATER_THAN: u8 = 10;
    pub const MINUS: u8 = 11;
    pub const BANG: u8 = 12;
    pub const JUMP_NOT_TRUTHY: u8 = 13;
    pub const JUMP: u8 = 14;
    pub const NULL: u8 = 15;
    pub const GET_GLOBAL: u8 = 16;
    pub const SET_GLOBAL: u8 = 17;
    pub const ARRAY: u8 = 18;
    pub const HASH: u8 = 19;
    pub const INDEX: u8 = 20;
    pub const CALL: u8 = 21;
    pub const RETURN_VALUE: u8 = 22;
    pub const RETURN: u8 = 23;
    pub const GET_LOCAL: u8 = 24;
    pub const SET_LOCAL: u8 = 25;
    pub const GET_BUILTIN: u8 = 26;
    pub const CLOSURE: u8 = 27;
}

#[derive(PartialEq, Eq)]
pub struct Disassembled(pub Instructions);

impl std::fmt::Debug for Disassembled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl std::fmt::Display for Disassembled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut rdr = Cursor::new(&self.0);
        let mut bytes_read = 0;
        loop {
            let opcode = match rdr.read_u8() {
                Err(ref e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(()),
                Err(e) => {
                    writeln!(f, "can't read from disassembly: err {e}")?;
                    return Err(std::fmt::Error);
                }
                Ok(opcode) => opcode,
            };
            let pc = bytes_read;
            bytes_read += 1;

            macro_rules! word {
                ($e: expr, $s: expr) => {{
                    let word = rdr.read_u16::<BigEndian>().expect($s);
                    bytes_read += 2;
                    writeln!(f, "{pc:04} {}", $e(word))?;
                }};
            }

            macro_rules! byte {
                ($e: expr, $s: expr) => {{
                    let byte = rdr.read_u8().expect($s);
                    bytes_read += 1;
                    writeln!(f, "{pc:04} {}", $e(byte))?;
                }};
            }

            match opcode {
                opcodes::CONSTANT => word!(Opcode::Constant, "u16 constant"),
                opcodes::JUMP_NOT_TRUTHY => word!(Opcode::JumpNotTruthy, "u16 address"),
                opcodes::JUMP => word!(Opcode::Jump, "u16 address"),
                opcodes::GET_GLOBAL => word!(Opcode::GetGlobal, "u16 constant"),
                opcodes::SET_GLOBAL => word!(Opcode::SetGlobal, "u16 constant"),
                opcodes::ARRAY => word!(Opcode::Array, "u16 length"),
                opcodes::HASH => word!(Opcode::Hash, "u16 length"),
                opcodes::CALL => byte!(Opcode::Call, "u8 params"),
                opcodes::GET_LOCAL => byte!(Opcode::GetLocal, "u8 constant"),
                opcodes::SET_LOCAL => byte!(Opcode::SetLocal, "u8 constant"),
                opcodes::GET_BUILTIN => byte!(Opcode::GetBuiltin, "u8 constant"),
                opcodes::CLOSURE => {
                    let closure_idx = rdr.read_u16::<BigEndian>().expect("u16 constant");
                    bytes_read += 2;
                    let free_vars = rdr.read_u8().expect("u8 constant");
                    bytes_read += 1;
                    writeln!(f, "{pc:04} {}", Opcode::Closure(closure_idx, free_vars))?;
                }
                op => writeln!(
                    f,
                    "{pc:04} {}",
                    match Opcode::try_from(op) {
                        Ok(code) => code.to_string(),
                        Err(err) => format!("unknown opcode {}", err.0),
                    }
                )?,
            };
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Opcode {
    Constant(u16),
    Add,
    Pop,
    Sub,
    Mul,
    Div,
    True,
    False,
    Equal,
    NotEqual,
    GreaterThan,
    Minus,
    Bang,
    JumpNotTruthy(u16),
    Jump(u16),
    Null,
    GetGlobal(u16),
    SetGlobal(u16),
    Array(u16),
    Hash(u16),
    Index,
    Call(u8),
    ReturnValue,
    Return,
    GetLocal(u8),
    SetLocal(u8),
    GetBuiltin(u8),
    Closure(u16, u8),
}

#[derive(Debug)]
pub struct InvalidOpcode(u8);

impl TryFrom<u8> for Opcode {
    type Error = InvalidOpcode;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            opcodes::CONSTANT => Ok(Self::Constant(0)),
            opcodes::ADD => Ok(Self::Add),
            opcodes::POP => Ok(Self::Pop),
            opcodes::SUB => Ok(Self::Sub),
            opcodes::MUL => Ok(Self::Mul),
            opcodes::DIV => Ok(Self::Div),
            opcodes::TRUE => Ok(Self::True),
            opcodes::FALSE => Ok(Self::False),
            opcodes::EQUAL => Ok(Self::Equal),
            opcodes::NOT_EQUAL => Ok(Self::NotEqual),
            opcodes::GREATER_THAN => Ok(Self::GreaterThan),
            opcodes::MINUS => Ok(Self::Minus),
            opcodes::BANG => Ok(Self::Bang),
            opcodes::JUMP_NOT_TRUTHY => Ok(Self::JumpNotTruthy(0)),
            opcodes::JUMP => Ok(Self::Jump(0)),
            opcodes::NULL => Ok(Self::Null),
            opcodes::INDEX => Ok(Self::Index),
            opcodes::GET_GLOBAL => Ok(Self::GetGlobal(0)),
            opcodes::SET_GLOBAL => Ok(Self::SetGlobal(0)),
            opcodes::GET_BUILTIN => Ok(Self::GetBuiltin(0)),
            op => Err(InvalidOpcode(op)),
        }
    }
}

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant(x) => write!(f, "OpConstant {x}"),
            Self::Add => write!(f, "OpAdd"),
            Self::Pop => write!(f, "OpPop"),
            Self::Sub => write!(f, "OpSub"),
            Self::Mul => write!(f, "OpMul"),
            Self::Div => write!(f, "OpDiv"),
            Self::True => write!(f, "OpTrue"),
            Self::False => write!(f, "OpFalse"),
            Self::Equal => write!(f, "OpEqual"),
            Self::NotEqual => write!(f, "OpNotEqual"),
            Self::GreaterThan => write!(f, "OpGreaterThan"),
            Self::Minus => write!(f, "OpMinus"),
            Self::Bang => write!(f, "OpBang"),
            Self::JumpNotTruthy(x) => write!(f, "OpJumpNotTruthy {x}"),
            Self::Jump(x) => write!(f, "OpJump {x}"),
            Self::Null => write!(f, "OpNull"),
            Self::GetGlobal(x) => write!(f, "OpGetGlobal {x}"),
            Self::SetGlobal(x) => write!(f, "OpSetGlobal {x}"),
            Self::Array(x) => write!(f, "OpArray {x}"),
            Self::Hash(x) => write!(f, "OpHash {x}"),
            Self::Index => write!(f, "OpIndex"),
            Self::Call(x) => write!(f, "OpCall {x}"),
            Self::ReturnValue => write!(f, "OpReturnValue"),
            Self::Return => write!(f, "OpReturn"),
            Self::GetLocal(x) => write!(f, "OpGetLocal {x}"),
            Self::SetLocal(x) => write!(f, "OpSetLocal {x}"),
            Self::GetBuiltin(x) => write!(f, "OpGetBuiltin {x}"),
            Self::Closure(x, y) => write!(f, "OpClosure {x} {y}"),
        }
    }
}

impl Opcode {
    pub const fn definition(self) -> (u8, usize) {
        match self {
            Self::Constant(_) => (opcodes::CONSTANT, 2),
            Self::Add => (opcodes::ADD, 0),
            Self::Pop => (opcodes::POP, 0),
            Self::Sub => (opcodes::SUB, 0),
            Self::Mul => (opcodes::MUL, 0),
            Self::Div => (opcodes::DIV, 0),
            Self::True => (opcodes::TRUE, 0),
            Self::False => (opcodes::FALSE, 0),
            Self::Equal => (opcodes::EQUAL, 0),
            Self::NotEqual => (opcodes::NOT_EQUAL, 0),
            Self::GreaterThan => (opcodes::GREATER_THAN, 0),
            Self::Minus => (opcodes::MINUS, 0),
            Self::Bang => (opcodes::BANG, 0),
            Self::JumpNotTruthy(_) => (opcodes::JUMP_NOT_TRUTHY, 2),
            Self::Jump(_) => (opcodes::JUMP, 2),
            Self::Null => (opcodes::NULL, 0),
            Self::GetGlobal(_) => (opcodes::GET_GLOBAL, 2),
            Self::SetGlobal(_) => (opcodes::SET_GLOBAL, 2),
            Self::Array(_) => (opcodes::ARRAY, 2),
            Self::Hash(_) => (opcodes::HASH, 2),
            Self::Index => (opcodes::INDEX, 0),
            Self::Call(_) => (opcodes::CALL, 1),
            Self::ReturnValue => (opcodes::RETURN_VALUE, 0),
            Self::Return => (opcodes::RETURN, 0),
            Self::GetLocal(_) => (opcodes::GET_LOCAL, 1),
            Self::SetLocal(_) => (opcodes::SET_LOCAL, 1),
            Self::GetBuiltin(_) => (opcodes::GET_BUILTIN, 1),
            Self::Closure(..) => (opcodes::CLOSURE, 2),
        }
    }
}

pub fn make(op: Opcode) -> Instructions {
    let (opcode, op_len) = op.definition();
    let mut v = Vec::with_capacity(op_len + 1);
    v.push(opcode);
    match op {
        Opcode::GetLocal(x) | Opcode::SetLocal(x) | Opcode::Call(x) | Opcode::GetBuiltin(x) => {
            v.write_u8(x).unwrap();
        }
        Opcode::Constant(x)
        | Opcode::JumpNotTruthy(x)
        | Opcode::Jump(x)
        | Opcode::GetGlobal(x)
        | Opcode::SetGlobal(x)
        | Opcode::Array(x)
        | Opcode::Hash(x) => {
            v.write_u16::<BigEndian>(x).unwrap();
        }
        Opcode::Closure(x, y) => {
            v.write_u16::<BigEndian>(x).unwrap();
            v.write_u8(y).unwrap();
        }
        Opcode::Add
        | Opcode::Pop
        | Opcode::Sub
        | Opcode::Mul
        | Opcode::Div
        | Opcode::True
        | Opcode::False
        | Opcode::Equal
        | Opcode::NotEqual
        | Opcode::GreaterThan
        | Opcode::Minus
        | Opcode::Bang
        | Opcode::Null
        | Opcode::Index
        | Opcode::ReturnValue
        | Opcode::Return => {}
    }

    v
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Opcode::Add),
            make(Opcode::GetLocal(255)),
            make(Opcode::Constant(2)),
            make(Opcode::Constant(65535)),
            make(Opcode::Closure(65535, 255)),
        ]
        .into_iter()
        .flatten()
        .collect::<Instructions>();
        let expected = r#"0000 OpAdd
0001 OpGetLocal 255
0003 OpConstant 2
0006 OpConstant 65535
0009 OpClosure 65535 255
"#;
        assert_eq!(Disassembled(instructions).to_string(), expected);
    }

    #[test]
    fn test_make() {
        let cases = vec![
            (Opcode::Constant(65534), vec![opcodes::CONSTANT, 255, 254]),
            (Opcode::Add, vec![opcodes::ADD]),
            (Opcode::GetLocal(255), vec![opcodes::GET_LOCAL, 255]),
            (
                Opcode::Closure(65534, 255),
                vec![opcodes::CLOSURE, 255, 254, 255],
            ),
        ];

        for (op, expected) in &cases {
            assert_eq!(&make(*op), expected);
        }
    }
}

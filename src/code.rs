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
                Err(e) => panic!("can't read from disassembly: err {e}"),
                Ok(opcode) => opcode,
            };
            let pc = bytes_read;
            bytes_read += 1;
            match opcode {
                opcodes::CONSTANT => {
                    let constant = rdr.read_u16::<BigEndian>().expect("u16 constant");
                    bytes_read += 2;
                    writeln!(f, "{pc:04} {}", Opcode::Constant(constant))?;
                }
                opcodes::JUMP_NOT_TRUTHY => {
                    let address = rdr.read_u16::<BigEndian>().expect("u16 address");
                    bytes_read += 2;
                    writeln!(f, "{pc:04} {}", Opcode::JumpNotTruthy(address))?;
                }
                opcodes::JUMP => {
                    let address = rdr.read_u16::<BigEndian>().expect("u16 address");
                    bytes_read += 2;
                    writeln!(f, "{pc:04} {}", Opcode::Jump(address))?;
                }
                opcodes::GET_GLOBAL => {
                    let constant = rdr.read_u16::<BigEndian>().expect("u16 constant");
                    bytes_read += 2;
                    writeln!(f, "{pc:04} {}", Opcode::GetGlobal(constant))?;
                }
                opcodes::SET_GLOBAL => {
                    let constant = rdr.read_u16::<BigEndian>().expect("u16 constant");
                    bytes_read += 2;
                    writeln!(f, "{pc:04} {}", Opcode::SetGlobal(constant))?;
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
            opcodes::GET_GLOBAL => Ok(Self::GetGlobal(0)),
            opcodes::SET_GLOBAL => Ok(Self::SetGlobal(0)),
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
        }
    }
}

pub fn make(op: Opcode) -> Instructions {
    let (opcode, op_len) = op.definition();
    let mut v = Vec::with_capacity(op_len + 1);
    v.push(opcode);
    match op {
        Opcode::Constant(x)
        | Opcode::JumpNotTruthy(x)
        | Opcode::Jump(x)
        | Opcode::GetGlobal(x)
        | Opcode::SetGlobal(x) => {
            v.write_u16::<BigEndian>(x).unwrap();
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
        | Opcode::Null => {}
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
            make(Opcode::Constant(2)),
            make(Opcode::Constant(65535)),
        ]
        .into_iter()
        .flatten()
        .collect::<Instructions>();
        let expected = r#"0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
"#;
        assert_eq!(Disassembled(instructions).to_string(), expected);
    }

    #[test]
    fn test_make() {
        let cases = vec![
            (Opcode::Constant(65534), vec![opcodes::CONSTANT, 255, 254]),
            (Opcode::Add, vec![opcodes::ADD]),
        ];

        for (op, expected) in &cases {
            assert_eq!(&make(*op), expected);
        }
    }
}

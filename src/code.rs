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
                    writeln!(f, "{pc:04} {}", Opcode::Constant(constant))?
                }
                opcodes::ADD => writeln!(f, "{pc:04} {}", Opcode::Add)?,
                opcodes::POP => writeln!(f, "{pc:04} {}", Opcode::Pop)?,
                opcodes::SUB => writeln!(f, "{pc:04} {}", Opcode::Sub)?,
                opcodes::MUL => writeln!(f, "{pc:04} {}", Opcode::Mul)?,
                opcodes::DIV => writeln!(f, "{pc:04} {}", Opcode::Div)?,
                opcodes::TRUE => writeln!(f, "{pc:04} {}", Opcode::True)?,
                opcodes::FALSE => writeln!(f, "{pc:04} {}", Opcode::False)?,
                opcodes::EQUAL => writeln!(f, "{pc:04} {}", Opcode::Equal)?,
                opcodes::NOT_EQUAL => writeln!(f, "{pc:04} {}", Opcode::NotEqual)?,
                opcodes::GREATER_THAN => writeln!(f, "{pc:04} {}", Opcode::GreaterThan)?,
                op => writeln!(f, "{pc:04} unknown opcode: {op}")?,
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
        }
    }
}

pub fn make(op: Opcode) -> Instructions {
    let (opcode, op_len) = op.definition();
    let mut v = Vec::with_capacity(op_len + 1);
    v.push(opcode);
    match op {
        Opcode::Constant(x) => {
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
        | Opcode::GreaterThan => {}
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

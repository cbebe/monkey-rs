use std::io::Cursor;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

pub type Instructions = Vec<u8>;

pub mod opcodes {
    pub const CONSTANT: u8 = 0;
    pub const ADD: u8 = 1;
    pub const POP: u8 = 2;
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
                    writeln!(f, "{pc:04} {}", Opcode::Constant(constant))?;
                    bytes_read += 2;
                }
                opcodes::ADD => {
                    writeln!(f, "{pc:04} {}", Opcode::Add)?;
                }
                opcodes::POP => {
                    writeln!(f, "{pc:04} {}", Opcode::Add)?;
                }
                op => panic!("unknown opcode: {op}"),
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Opcode {
    Constant(u16),
    Add,
    Pop,
}

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant(x) => write!(f, "OpConstant {x}"),
            Self::Add => write!(f, "OpAdd"),
            Self::Pop => write!(f, "OpPop"),
        }
    }
}

impl Opcode {
    pub const fn definition(self) -> (u8, usize) {
        match self {
            Self::Constant(_) => (opcodes::CONSTANT, 2),
            Self::Add => (opcodes::ADD, 0),
            Self::Pop => (opcodes::POP, 0),
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
        Opcode::Add | Opcode::Pop => {}
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

        for (op, expected) in cases.iter() {
            assert_eq!(&make(*op), expected);
        }
    }
}

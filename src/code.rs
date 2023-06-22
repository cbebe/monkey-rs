use std::io::Cursor;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

pub type Instructions = Vec<u8>;

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
                Err(e) => panic!("Can't read from disassembly: err {}", e),
                Ok(opcode) => opcode,
            };
            let pc = bytes_read;
            bytes_read += 1;
            match opcode {
                0 => {
                    let constant = rdr.read_u16::<BigEndian>().expect("u16 constant");
                    writeln!(f, "{:04} {}", pc, Opcode::Constant(constant))?;
                    bytes_read += 2;
                }
                op => panic!("unknown opcode: {}", op),
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Opcode {
    Constant(u16),
}

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Constant(x) => write!(f, "OpConstant {}", x),
        }
    }
}

impl Opcode {
    pub fn definition(self) -> (u8, usize) {
        match self {
            Opcode::Constant(_) => (0, 2),
        }
    }
}

pub fn make(op: &Opcode) -> Instructions {
    let (opcode, op_len) = op.definition();
    let mut v = Vec::with_capacity(op_len + 1);
    v.push(opcode);
    match op {
        Opcode::Constant(x) => {
            v.write_u16::<BigEndian>(*x).unwrap();
        }
    }

    v
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(&Opcode::Constant(1)),
            make(&Opcode::Constant(2)),
            make(&Opcode::Constant(65535)),
        ]
        .into_iter()
        .flatten()
        .collect::<Instructions>();
        let expected = r#"0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
"#;
        assert_eq!(Disassembled(instructions).to_string(), expected);
    }

    #[test]
    fn test_make() {
        let cases = vec![(Opcode::Constant(65534), vec![0, 255, 254])];

        for (op, expected) in cases.iter() {
            assert_eq!(&make(op), expected);
        }
    }
}

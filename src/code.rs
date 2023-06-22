use byteorder::{BigEndian, WriteBytesExt};

pub type Instructions = Vec<u8>;

#[derive(Debug, Copy, Clone)]
enum Opcode {
    Constant(u16),
}

impl Opcode {
    pub fn definition(self) -> (u8, usize) {
        match self {
            Opcode::Constant(_) => (0, 2),
        }
    }
}

fn make(op: &Opcode) -> Instructions {
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
    fn test_make() {
        let cases: Vec<(Opcode, Vec<u8>)> = vec![(Opcode::Constant(65534), vec![0, 255, 254])];

        for (op, expected) in cases.iter() {
            assert_eq!(&make(op), expected);
        }
    }
}

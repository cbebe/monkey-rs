use crate::{ast, code, object::Object};

#[derive(Debug)]
pub enum Node<'a> {
    Statement(ast::Statement<'a>),
    Expression(ast::Expression<'a>),
}

impl<'a> std::fmt::Display for Node<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug)]
pub enum Error {
    NotYetImplemented(String),
}

impl<'a> std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

pub struct Compiler {
    instructions: code::Instructions,
    constants: Vec<Object>,
}

pub struct Bytecode {
    pub instructions: code::Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub const fn new() -> Self {
        Self {
            instructions: vec![],
            constants: vec![],
        }
    }

    fn add_instruction(&mut self, ins: &mut code::Instructions) -> usize {
        let pos = self.instructions.len();
        self.instructions.append(ins);
        pos
    }

    fn emit(&mut self, op: code::Opcode) -> usize {
        let mut ins = code::make(op);
        self.add_instruction(&mut ins)
    }

    fn add_constant(&mut self, obj: Object) -> u16 {
        self.constants.push(obj);
        (self.constants.len() - 1)
            .try_into()
            .expect("number of constants exceeded")
    }

    pub fn compile_program(&mut self, program: ast::Program) -> Result<(), Error> {
        for statement in program.0 .0 {
            self.compile(Node::Statement(statement))?;
        }
        Ok(())
    }

    fn compile(&mut self, node: Node) -> Result<(), Error> {
        use ast::{Expression, Literal, Statement};
        match node {
            Node::Statement(Statement::Expression(e)) => {
                self.compile(Node::Expression(e))?;
                self.emit(code::Opcode::Pop);
            }
            Node::Expression(Expression::Infix(left, op, right)) => {
                // Reverse operand emit so we can use the same instruction as GT
                if op == ast::Binary::LT {
                    self.compile(Node::Expression(*right))?;
                    self.compile(Node::Expression(*left))?;
                } else {
                    self.compile(Node::Expression(*left))?;
                    self.compile(Node::Expression(*right))?;
                }
                match op {
                    ast::Binary::Add => self.emit(code::Opcode::Add),
                    ast::Binary::Sub => self.emit(code::Opcode::Sub),
                    ast::Binary::Mul => self.emit(code::Opcode::Mul),
                    ast::Binary::Div => self.emit(code::Opcode::Div),
                    ast::Binary::LT | ast::Binary::GT => self.emit(code::Opcode::GreaterThan),
                    ast::Binary::Eq => self.emit(code::Opcode::Equal),
                    ast::Binary::Neq => self.emit(code::Opcode::NotEqual),
                };
            }
            Node::Expression(Expression::Literal(Literal::Boolean(bool))) => {
                self.emit(if bool {
                    code::Opcode::True
                } else {
                    code::Opcode::False
                });
            }
            Node::Expression(Expression::Literal(Literal::Integer(int))) => {
                let int_obj = Object::Integer(int);
                let idx = self.add_constant(int_obj);
                self.emit(code::Opcode::Constant(idx));
            }
            e => return Err(Error::NotYetImplemented(e.to_string())),
        }
        Ok(())
    }

    #[allow(clippy::missing_const_for_fn)]
    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::{self, Disassembled, Instructions, Opcode},
        util::test_utils::{self, compile_program, test_constants},
    };

    type Test<'a> = (&'a str, Vec<test_utils::Constant>, Vec<Instructions>);

    fn make(ops: Vec<Opcode>) -> Vec<Instructions> {
        ops.into_iter().map(code::make).collect()
    }

    #[test]
    fn test_integer_arithmetic() {
        use code::Opcode::{Add, Constant, Div, Equal, False, GreaterThan, Mul, NotEqual, Pop, Sub, True};
        use test_utils::Constant::Int;
        run_compiler_tests(vec![
            (
                "1 + 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), Add, Pop]),
            ),
            (
                "1; 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Pop, Constant(1), Pop]),
            ),
            (
                "1 - 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), Sub, Pop]),
            ),
            (
                "1 * 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), Mul, Pop]),
            ),
            (
                "2 / 1",
                vec![Int(2), Int(1)],
                make(vec![Constant(0), Constant(1), Div, Pop]),
            ),
            ("true", vec![], make(vec![True, Pop])),
            ("false", vec![], make(vec![False, Pop])),
            (
                "1 > 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), GreaterThan, Pop]),
            ),
            (
                "1 < 2",
                vec![Int(2), Int(1)],
                make(vec![Constant(0), Constant(1), GreaterThan, Pop]),
            ),
            (
                "1 == 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), Equal, Pop]),
            ),
            (
                "1 != 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), NotEqual, Pop]),
            ),
            ("true == false", vec![], make(vec![True, False, Equal, Pop])),
            (
                "true != false",
                vec![],
                make(vec![True, False, NotEqual, Pop]),
            ),
        ]);
    }

    fn run_compiler_tests(tests: Vec<Test>) {
        for (input, constants, instructions) in tests {
            let bytecode = compile_program(input);
            test_instructions(instructions, bytecode.instructions);
            if let Err(err) = test_constants(constants, bytecode.constants) {
                panic!("failed test for input {}. {err}", &input);
            }
        }
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let expected = expected.into_iter().flatten().collect::<Instructions>();
        assert_eq!(Disassembled(expected), Disassembled(actual));
    }
}

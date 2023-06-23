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
    UnknownOperation(ast::Binary),
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
                self.compile(Node::Expression(*left))?;
                self.compile(Node::Expression(*right))?;
                match op {
                    ast::Binary::Add => self.emit(code::Opcode::Add),
                    ast::Binary::Sub => self.emit(code::Opcode::Sub),
                    ast::Binary::Mul => self.emit(code::Opcode::Mul),
                    ast::Binary::Div => self.emit(code::Opcode::Div),
                    // ast::Binary::LT => todo!(),
                    // ast::Binary::GT => todo!(),
                    // ast::Binary::Eq => todo!(),
                    // ast::Binary::Neq => todo!(),
                    op => return Err(Error::UnknownOperation(op)),
                };
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
        code::{make, Disassembled, Instructions, Opcode},
        util::test_utils::{compile_program, test_constants, Constant},
    };

    struct Test<'a> {
        input: &'a str,
        constants: Vec<Constant>,
        instructions: Vec<Instructions>,
    }

    #[test]
    fn test_integer_arithmetic() {
        run_compiler_tests(vec![
            Test {
                input: "1 + 2",
                constants: vec![Constant::Int(1), Constant::Int(2)],
                instructions: vec![
                    make(Opcode::Constant(0)),
                    make(Opcode::Constant(1)),
                    make(Opcode::Add),
                    make(Opcode::Pop),
                ],
            },
            Test {
                input: "1; 2",
                constants: vec![Constant::Int(1), Constant::Int(2)],
                instructions: vec![
                    make(Opcode::Constant(0)),
                    make(Opcode::Pop),
                    make(Opcode::Constant(1)),
                    make(Opcode::Pop),
                ],
            },
            Test {
                input: "1 - 2",
                constants: vec![Constant::Int(1), Constant::Int(2)],
                instructions: vec![
                    make(Opcode::Constant(0)),
                    make(Opcode::Constant(1)),
                    make(Opcode::Sub),
                    make(Opcode::Pop),
                ],
            },
            Test {
                input: "1 * 2",
                constants: vec![Constant::Int(1), Constant::Int(2)],
                instructions: vec![
                    make(Opcode::Constant(0)),
                    make(Opcode::Constant(1)),
                    make(Opcode::Mul),
                    make(Opcode::Pop),
                ],
            },
            Test {
                input: "2 / 1",
                constants: vec![Constant::Int(2), Constant::Int(1)],
                instructions: vec![
                    make(Opcode::Constant(0)),
                    make(Opcode::Constant(1)),
                    make(Opcode::Div),
                    make(Opcode::Pop),
                ],
            },
        ]);
    }

    fn run_compiler_tests(tests: Vec<Test>) {
        for test in tests {
            let bytecode = compile_program(&test.input);
            test_instructions(test.instructions, bytecode.instructions);
            if let Err(err) = test_constants(test.constants, bytecode.constants) {
                panic!("failed test for input {}. {err}", test.input);
            }
        }
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let expected = expected.into_iter().flatten().collect::<Instructions>();
        assert_eq!(Disassembled(expected), Disassembled(actual));
    }
}

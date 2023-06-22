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
            Node::Statement(Statement::Expression(e)) => self.compile(Node::Expression(e))?,
            Node::Expression(Expression::Infix(left, op, right)) => {
                self.compile(Node::Expression(*left))?;
                self.compile(Node::Expression(*right))?;
                match op {
                    ast::Binary::Add => self.emit(code::Opcode::Add),
                    // ast::Binary::Sub => todo!(),
                    // ast::Binary::Mul => todo!(),
                    // ast::Binary::Div => todo!(),
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
        object, parser,
    };

    use super::Compiler;

    enum Constant {
        Int(i64),
    }

    struct Test<'a> {
        input: &'a str,
        constants: Vec<Constant>,
        instructions: Vec<Instructions>,
    }

    #[test]
    fn test_integer_arithmetic() {
        run_compiler_tests(vec![Test {
            input: "1 + 2",
            constants: vec![Constant::Int(1), Constant::Int(2)],
            instructions: vec![
                make(Opcode::Constant(0)),
                make(Opcode::Constant(1)),
                make(Opcode::Add),
            ],
        }]);
    }

    fn run_compiler_tests(tests: Vec<Test>) {
        for test in tests {
            let program = match parser::program(&test.input) {
                Ok(p) => p.1,
                Err(e) => panic!("invalid program {e}"),
            };

            let mut compiler = Compiler::new();
            if let Err(err) = compiler.compile_program(program) {
                panic!("compiler error: {err:?}");
            };
            let bytecode = compiler.bytecode();
            test_instructions(test.instructions, bytecode.instructions);
            test_constants(test.constants, bytecode.constants);
        }
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let expected = expected.into_iter().flatten().collect::<Instructions>();
        assert_eq!(Disassembled(expected), Disassembled(actual));
    }

    fn test_constants(expected: Vec<Constant>, actual: Vec<object::Object>) {
        assert_eq!(expected.len(), actual.len());
        for (want, got) in expected.iter().zip(actual.iter()) {
            assert!(match (want, got) {
                (Constant::Int(x), object::Object::Integer(y)) if x == y => true,
                _ => false,
            });
        }
    }
}

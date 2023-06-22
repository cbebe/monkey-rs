use crate::{ast, code, object::Object};

#[derive(Debug)]
pub enum Error {}

pub struct Compiler {
    instructions: code::Instructions,
    constants: Vec<Object>,
}

pub struct Bytecode {
    instructions: code::Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
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
        let mut ins = code::make(&op);
        self.add_instruction(&mut ins)
    }

    fn add_constant(&mut self, obj: Object) -> u16 {
        self.constants.push(obj);
        (self.constants.len() - 1)
            .try_into()
            .expect("number of constants exceeded")
    }

    pub fn compile(&mut self, node: ast::Node) -> Result<(), Error> {
        use ast::{Expression, Literal, Node, Statement};
        match node {
            Node::Program(prog) => {
                for statement in prog.0 .0.into_iter() {
                    self.compile(Node::Statement(statement))?;
                }
            }
            Node::Statement(Statement::Expression(e)) => self.compile(Node::Expression(e))?,
            Node::Expression(Expression::Infix(left, _op, right)) => {
                self.compile(Node::Expression(*left))?;
                // TODO: parse op
                self.compile(Node::Expression(*right))?;
            }
            Node::Expression(Expression::Literal(Literal::Integer(int))) => {
                let int_obj = Object::Integer(int);
                let idx = self.add_constant(int_obj);
                self.emit(code::Opcode::Constant(idx));
            }
            e => panic!("not yet implemented {e:?}"),
        }
        Ok(())
    }

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
        ast::Node,
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
            instructions: vec![make(&Opcode::Constant(0)), make(&Opcode::Constant(1))],
        }]);
    }

    fn run_compiler_tests(tests: Vec<Test>) {
        for test in tests {
            let program = match parser::program(&test.input) {
                Ok(p) => p.1,
                Err(e) => panic!("invalid program {e}"),
            };

            let mut compiler = Compiler::new();
            if let Err(err) = compiler.compile(Node::Program(program)) {
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

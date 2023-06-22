use crate::{ast, code, object};

#[derive(Debug)]
pub enum Error {}

pub struct Compiler {
    instructions: code::Instructions,
    constants: Vec<object::Object>,
}

pub struct Bytecode {
    instructions: code::Instructions,
    constants: Vec<object::Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            constants: vec![],
        }
    }

    pub fn compile(&mut self, node: ast::Node) -> Result<(), Error> {
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
    use crate::{ast::Node, code::Instructions, object, parser};

    use super::Compiler;

    enum Constant {
        Int(i64),
    }
    struct Test {
        input: String,
        constants: Vec<Constant>,
        instructions: Vec<Instructions>,
    }

    #[test]
    fn test_integer_arithmetic() {
        todo!()
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
        todo!()
    }

    fn test_constants(expected: Vec<Constant>, actual: Vec<object::Object>) {
        todo!()
    }
}

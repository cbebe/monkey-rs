pub fn str_vec(a: &[impl ToString]) -> Vec<String> {
    a.iter().map(std::string::ToString::to_string).collect()
}

#[cfg(test)]
pub mod test_utils {
    use std::rc::Rc;

    use crate::{
        compiler::{Bytecode, Compiler},
        object::Object,
        parser,
    };
    #[derive(Debug)]
    pub enum Constant {
        Int(i64),
        Bool(bool),
        String(Rc<str>),
        Null,
    }

    pub fn test_object(got: &Object, want: &Constant) -> Result<(), String> {
        match (want, got) {
            (Constant::Int(x), Object::Integer(y)) if x == y => Ok(()),
            (Constant::Bool(x), Object::Boolean(y)) if x == y => Ok(()),
            (Constant::String(x), Object::String(y)) if x.as_ref() == y => Ok(()),
            (Constant::Null, Object::Null) => Ok(()),
            _ => Err(format!("want: {want:?}\ngot: {got:?}")),
        }
    }

    pub fn test_constants(expected: Vec<Constant>, actual: Vec<Object>) -> Result<(), String> {
        assert_eq!(expected.len(), actual.len());
        for (want, got) in expected.iter().zip(actual.iter()) {
            test_object(got, want)?;
        }
        Ok(())
    }

    pub fn compile_program(input: &str) -> Bytecode {
        let program = match parser::program(input) {
            Ok(p) => p.1,
            Err(e) => panic!("invalid program {e}"),
        };

        let mut compiler = Compiler::new();
        if let Err(err) = compiler.compile_program(program) {
            panic!("compiler error: {err:#?}");
        };
        compiler.bytecode()
    }
}

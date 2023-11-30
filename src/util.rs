pub fn str_vec(a: &[impl ToString]) -> Vec<String> {
    a.iter().map(std::string::ToString::to_string).collect()
}

#[cfg(test)]
pub mod test_utils {
    use std::{collections::BTreeMap, rc::Rc};

    use crate::{
        compiler::{Bytecode, Compiler},
        object::{Hashable, Object},
        parser,
    };

    // Why did I make life hard for myself by having two different types for objects???
    // And it's just used for tests??
    // smh charles
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Constant {
        Int(i64),
        Bool(bool),
        String(Rc<str>),
        Null,
        Array(Vec<Constant>),
        Hash(BTreeMap<Constant, Constant>),
    }

    pub fn test_object(got: &Object, want: &Constant) -> Result<(), String> {
        match (want, got) {
            (Constant::Int(x), Object::Integer(y)) if x == y => Ok(()),
            (Constant::Bool(x), Object::Boolean(y)) if x == y => Ok(()),
            (Constant::String(x), Object::String(y)) if x.as_ref() == y => Ok(()),
            (Constant::Array(x), Object::Array(y)) => test_constants(x, y).map_err(|err| {
                format!("failed array test at: {err}\nwant: {want:?}\ngot: {got:?}")
            }),
            (Constant::Hash(x), Object::Hash(y)) => {
                if x.len() != y.len() {
                    return Err(format!(
                        "failed hash test: mismatched lengths\nwant: {}\ngot: {}",
                        x.len(),
                        y.len()
                    ));
                }

                for (k, v) in x {
                    let key = match k {
                        Constant::Int(x) => x.hash_key(),
                        Constant::Bool(x) => x.hash_key(),
                        Constant::String(x) => x.to_string().hash_key(),
                        Constant::Null | Constant::Array(_) | Constant::Hash(_) => {
                            panic!("invalid hash test: non-hashable as key")
                        }
                    };
                    if let Some(item) = y.get(&key) {
                        test_object(&item.key, k)?;
                        test_object(&item.value, v)?;
                    } else {
                        return Err(format!("key {:?} not found", k));
                    }
                }

                Ok(())
            }
            (Constant::Null, Object::Null) => Ok(()),
            _ => Err(format!("want: {want:?}\ngot: {got:?}")),
        }
    }

    pub fn test_constants(expected: &[Constant], actual: &[Object]) -> Result<(), String> {
        if expected.len() != actual.len() {
            return Err(format!(
                "wrong number of elements. want: {}, got: {}",
                expected.len(),
                actual.len()
            ));
        }
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

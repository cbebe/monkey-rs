use crate::{object::Object, vm};

type Result = std::result::Result<Option<Object>, vm::Error>;
pub struct Builtin(pub &'static str, pub fn(args: &[Object]) -> Result);

macro_rules! check_args {
    ($args: expr, $want: expr) => {
        if $args.len() != $want {
            return Err(vm::Error::WrongArguments {
                want: $want,
                got: $args.len(),
            });
        }
    };
}

fn len(args: &[Object]) -> Result {
    check_args!(args, 1);
    macro_rules! size {
        ($x: expr) => {{
            Ok(Some(Object::Integer($x.len().try_into().unwrap())))
        }};
    }
    match &args[0] {
        Object::String(x) => size!(x),
        Object::Array(x) => size!(x),
        Object::Hash(x) => size!(x),
        Object::Null
        | Object::Closure { .. }
        | Object::Function(_)
        | Object::Builtin(_)
        | Object::Integer(_)
        | Object::Boolean(_) => Err(vm::Error::InvalidUnary(args[0].clone())),
    }
}

// Need to follow the same signature as the others
#[allow(clippy::unnecessary_wraps)]
fn puts(args: &[Object]) -> Result {
    for i in args.iter() {
        println!("{i}");
    }
    Ok(None)
}

fn _array_builtin(args: &[Object]) -> std::result::Result<&Vec<Object>, vm::Error> {
    match &args[0] {
        Object::Array(x) => Ok(x),
        Object::Null
        | Object::Closure { .. }
        | Object::Hash(_)
        | Object::Function(_)
        | Object::Builtin(_)
        | Object::Integer(_)
        | Object::String(_)
        | Object::Boolean(_) => Err(vm::Error::InvalidUnary(args[0].clone())),
    }
}

fn first(args: &[Object]) -> Result {
    check_args!(args, 1);
    Ok(_array_builtin(args)?.first().cloned())
}

fn last(args: &[Object]) -> Result {
    check_args!(args, 1);
    Ok(_array_builtin(args)?.last().cloned())
}

fn rest(args: &[Object]) -> Result {
    check_args!(args, 1);
    let arr = _array_builtin(args)?;
    Ok((arr.len() > 1).then(|| Object::Array(arr[1..].to_vec())))
}

fn push(args: &[Object]) -> Result {
    check_args!(args, 2);
    let mut arr = _array_builtin(args)?.clone();
    arr.push(args[1].clone());
    Ok(Some(Object::Array(arr)))
}

macro_rules! builtin {
    ($($builtin: tt),*) => {{
        [$(Builtin(stringify!($builtin), $builtin)),*]
        }
    };
}

pub const BUILTINS: [Builtin; 6] = builtin!(len, puts, first, last, rest, push);

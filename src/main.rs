#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
#![allow(clippy::implicit_return, clippy::question_mark_used)]

use nom::error::VerboseError;
use rustyline::{self, error::ReadlineError, DefaultEditor};

mod ast;
mod code;
mod compiler;
mod object;
mod parser;
mod util;
mod vm;

#[derive(Debug)]
enum Error {
    Parser(String),
    Rustyline(ReadlineError),
    Compiler(String),
    VM(vm::Error),
    NoFileGiven,
    FileError,
}

impl<'a> From<nom::Err<VerboseError<&'a str>>> for Error {
    fn from(value: nom::Err<VerboseError<&'a str>>) -> Self {
        Self::Parser(value.to_string())
    }
}

impl From<ReadlineError> for Error {
    fn from(value: ReadlineError) -> Self {
        Self::Rustyline(value)
    }
}

impl From<compiler::Error> for Error {
    fn from(value: compiler::Error) -> Self {
        Self::Compiler(value.to_string())
    }
}

impl From<vm::Error> for Error {
    fn from(value: vm::Error) -> Self {
        Self::VM(value)
    }
}

#[allow(dead_code)]
fn read_file<'a>() -> Result<(), Error> {
    let args = std::env::args().collect::<Vec<String>>();
    let path = args.get(1).ok_or(Error::NoFileGiven)?;
    let program = std::fs::read_to_string(path).or(Err(Error::FileError))?;
    let (_, ast) = parser::program(&program)?;
    println!("{ast}");
    Ok(())
}

fn process_line(line: &str, use_ast: bool) -> Result<(), Error> {
    let (_, program) = parser::program(line)?;
    if use_ast {
        println!("{program:#?}");
        return Ok(());
    }
    let mut comp = compiler::Compiler::new();
    comp.compile_program(program)?;
    let machine = vm::VM::new(comp.bytecode()).run()?;
    let elem = machine.last_popped();
    if let Some(obj) = elem {
        println!("{obj}");
    }
    Ok(())
}

fn main() -> Result<(), Error> {
    let args = std::env::args().collect::<Vec<String>>();
    let use_ast = matches!(args.get(1).map(|e| &**e), Some("ast"));
    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline("");
        match readline {
            Ok(line) => {
                if line.is_empty() {
                    continue;
                }
                if let Err(err) = process_line(&line, use_ast) {
                    println!("{err:?}");
                };
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Error: {err:?}");
                break;
            }
        }
    }
    Ok(())
}

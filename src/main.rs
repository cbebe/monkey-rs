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

fn process_line(line: &str) -> Result<(), Error> {
    let (_, program) = parser::program(&line)?;
    let mut comp = compiler::Compiler::new();
    comp.compile_program(program)?;
    let mut machine = vm::VM::new(comp.bytecode());
    machine.run()?;
    let stack_top = machine.stack_top();
    if let Some(obj) = stack_top {
        println!("{obj}");
    }
    Ok(())
}

fn main() -> Result<(), Error> {
    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline("");
        match readline {
            Ok(line) => {
                if line.is_empty() {
                    continue;
                }
                if let Err(err) = process_line(&line) {
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

#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
#![allow(
    clippy::implicit_return,
    clippy::question_mark_used,
    clippy::cast_possible_truncation
)]

mod ast;
mod code;
mod compiler;
#[cfg(test)]
mod compiler_tests;
mod object;
mod parser;
mod symbol_table;
mod util;
mod vm;

use nom::error::VerboseError;
use rustyline::{self, error::ReadlineError};

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
fn read_file() -> Result<(), Error> {
    let args = std::env::args().collect::<Vec<String>>();
    let path = args.get(1).ok_or(Error::NoFileGiven)?;
    let program = std::fs::read_to_string(path).or(Err(Error::FileError))?;
    let (_, ast) = parser::program(&program)?;
    println!("{ast}");
    Ok(())
}

fn main() -> Result<(), Error> {
    let args = std::env::args().collect::<Vec<String>>();
    let use_ast = matches!(args.get(1).map(|e| &**e), Some("ast"));
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut constants = Vec::<object::Object>::new();
    let mut globals = Vec::<object::Object>::with_capacity(vm::GLOBALS_SIZE);
    let mut symbol_table = symbol_table::SymbolTable::new();
    loop {
        let readline = rl.readline("");
        match readline {
            Ok(line) => {
                if line.is_empty() {
                    continue;
                }
                if let Err(err) = {
                    let line: &str = &line;
                    let (_, program) = parser::program(line)?;
                    if use_ast {
                        println!("{program:#?}");
                    } else {
                        // May the Rust gods forgive me
                        let mut comp = compiler::Compiler::new()
                            .with_state(symbol_table.clone(), constants.clone());
                        comp.compile_program(program)?;
                        symbol_table = comp.symbol_table.clone();
                        let code = comp.bytecode();
                        constants = code.constants.clone();
                        let machine = vm::VM::new(code).with_globals(globals.clone()).run()?;
                        globals = machine.globals.clone().unwrap();
                        let elem = machine.last_popped();
                        if let Some(obj) = elem {
                            println!("{obj}");
                        }
                    }
                    Ok::<(), Error>(())
                } {
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

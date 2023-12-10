use nom::error::VerboseError;
use rustyline::{self, error::ReadlineError};

#[derive(Debug)]
pub enum Error {
    Parser(String),
    Rustyline(ReadlineError),
    Compiler(String),
    VM(crate::vm::Error),
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

impl From<crate::compiler::Error> for Error {
    fn from(value: crate::compiler::Error) -> Self {
        Self::Compiler(value.to_string())
    }
}

impl From<crate::vm::Error> for Error {
    fn from(value: crate::vm::Error) -> Self {
        Self::VM(value)
    }
}

#[allow(dead_code)]
fn read_file() -> Result<(), Error> {
    let args = std::env::args().collect::<Vec<String>>();
    let path = args.get(1).ok_or(Error::NoFileGiven)?;
    let program = std::fs::read_to_string(path).or(Err(Error::FileError))?;
    let (_, ast) = crate::parser::program(&program)?;
    println!("{ast}");
    Ok(())
}

pub fn repl() -> Result<(), Error> {
    let args = std::env::args().collect::<Vec<String>>();
    let use_ast = matches!(args.get(1).map(|e| &**e), Some("ast"));
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut constants = Vec::<crate::object::Object>::new();
    let mut globals = Vec::<crate::object::Object>::with_capacity(crate::vm::GLOBALS_SIZE);
    let mut symbol_table = crate::symbol_table::SymbolTable::default();
    loop {
        let readline = rl.readline("");
        match readline {
            Ok(line) => {
                if line.is_empty() {
                    continue;
                }
                if let Err(err) = {
                    let line: &str = &line;
                    let (_, program) = crate::parser::program(line)?;
                    if use_ast {
                        println!("{program:#?}");
                    } else {
                        // May the Rust gods forgive me
                        let mut comp = crate::compiler::Compiler::new()
                            .with_state(symbol_table.clone(), constants.clone());
                        comp.compile_program(program)?;
                        symbol_table = comp.symbol_table.borrow_mut().clone();
                        let code = comp.bytecode();
                        constants = code.constants.clone();
                        let machine = crate::vm::VM::new(code)
                            .with_globals(globals.clone())
                            .run()?;
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

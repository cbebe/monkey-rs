#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
#![allow(clippy::implicit_return, clippy::question_mark_used)]

use nom::error::VerboseError;

mod ast;
mod parser;

#[derive(Debug)]
enum Error {
    Parser(String),
    NoFileGiven,
    FileError,
}

impl<'a> From<nom::Err<VerboseError<&'a str>>> for Error {
    fn from(value: nom::Err<VerboseError<&'a str>>) -> Self {
        Self::Parser(value.to_string())
    }
}

fn main() -> Result<(), Error> {
    let args = std::env::args().collect::<Vec<String>>();
    let path = args.get(1).ok_or(Error::NoFileGiven)?;
    let program = std::fs::read_to_string(&path).or(Err(Error::FileError))?;
    let (_, ast) = parser::program(&program)?;
    println!("{ast}");
    Ok(())
}

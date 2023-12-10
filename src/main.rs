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
mod repl;
mod symbol_table;
mod util;
mod vm;
#[cfg(test)]
mod vm_tests;

fn main() -> Result<(), repl::Error> {
    repl::repl()
}

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate thiserror;
#[macro_use]
extern crate derive_new;

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

use anyhow::Result;
use std::io;
use whoami;

fn main() -> Result<()> {
    let username = whoami::username();
    println!(
        "Hello {}! This is the Monkey programming language!",
        username
    );
    println!("Feel free tot type in commands");

    let mut stdin = io::BufReader::new(io::stdin());
    let mut stdout = io::BufWriter::new(io::stdout());
    repl::start(&mut stdin, &mut stdout)?;
    Ok(())
}

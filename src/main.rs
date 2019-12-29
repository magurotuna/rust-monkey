#[macro_use]
extern crate lazy_static;

mod ast;
mod lexer;
mod repl;
mod token;

use std::io;
use whoami;

fn main() {
    let username = whoami::username();
    println!(
        "Hello {}! This is the Monkey programming language!",
        username
    );
    println!("Feel free tot type in commands");

    let mut stdin = io::BufReader::new(io::stdin());
    let mut stdout = io::BufWriter::new(io::stdout());
    repl::start(&mut stdin, &mut stdout).expect("Fatal error happened!");
}

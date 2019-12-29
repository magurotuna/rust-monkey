use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io;
use std::io::prelude::*;

lazy_static! {
    static ref PROMPT: &'static str = ">> ";
}

pub fn start<R: io::Read, W: io::Write>(
    input: &mut io::BufReader<R>,
    output: &mut io::BufWriter<W>,
) -> io::Result<()> {
    loop {
        write!(output, "{}", *PROMPT)?;
        output.flush()?;

        let mut line = String::new();
        input.read_line(&mut line)?;
        let mut lexer = Lexer::new(line);

        let mut tok = lexer.next_token();
        while tok.token_type != TokenType::Eof {
            writeln!(output, "{:?}", &tok)?;
            output.flush()?;
            tok = lexer.next_token();
        }
    }
}

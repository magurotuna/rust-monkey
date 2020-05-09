use crate::lexer::Lexer;
use crate::parser::Parser;
use lazy_static::lazy_static;
use std::io;
use std::io::prelude::*;

lazy_static! {
    static ref PROMPT: &'static str = ">> ";
    static ref MONKEY_FACE: &'static str = r#"
　　　　 _)ヽ　　 n))
　　　_／＿＿＼_ ｜｜
　　 (6 (●●) 6)/ /
　　　人( ‥ )人　/
　　 /　＼⊥／　／
　　｜｜| ￣ | /
　　/ ∧ヽ＿ノ/
　／／／ ＿＿ ＼
（(U (　(　　)　)
　￣　≧_)　(_≦
    "#;
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
        let lexer = Lexer::new(line);
        let parser = Parser::new(lexer);

        match parser.parse_program() {
            Ok(program) => {
                writeln!(output, "{}", program)?;
            }
            Err(err) => {
                writeln!(
                    output,
                    "{}\nWoops! We ran into some monkey business here!",
                    *MONKEY_FACE
                )?;
                writeln!(output, "parse errors:\n{}", err)?;
            }
        };
        output.flush()?;
    }
}

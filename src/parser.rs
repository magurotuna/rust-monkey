use crate::ast;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::default::Default;
use std::mem;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            cur_token: Token::default(),
            peek_token: Token::default(),
        };
        // Load 2 tokens to set both cur_token & peek_token
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.cur_token = mem::replace(&mut self.peek_token, Token::default());
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&self) -> ast::Program {
        todo!();
    }
}

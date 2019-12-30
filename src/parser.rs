use crate::ast;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use anyhow::{anyhow, Result};
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

    fn parse_program(&mut self) -> Result<ast::Node> {
        let mut statements = Vec::new();
        while self.cur_token.token_type != TokenType::Eof {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }
        Ok(ast::Node::Program(statements))
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            _ => Err(anyhow!(
                "Statement parse error. cur_token: {:?}, peek_token: {:?}",
                self.cur_token,
                self.peek_token
            )),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement> {
        if !self.expect_peek(&TokenType::Identifier) {
            return Err(anyhow!(
                "Let statement parse error. cur_token: {:?}, peek_token: {:?}",
                self.cur_token,
                self.peek_token
            ));
        }

        let identifier = ast::Identifier {
            token: self.cur_token.clone(),         // cannot get rid of `clone`?
            value: self.cur_token.literal.clone(), // cannot get rid of `clone`?
        };

        if !self.expect_peek(&TokenType::Assign) {
            return Err(anyhow!(
                "Let statement parse error. cur_token: {:?}, peek_token: {:?}",
                self.cur_token,
                self.peek_token
            ));
        }

        // TODO: skip to read the expression until encountering semicolon.
        while !self.cur_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ast::Statement::Let(identifier, ast::Expression::Dummy)) // FIXME
    }

    fn cur_token_is(&self, t: &TokenType) -> bool {
        self.cur_token.token_type == *t
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        self.peek_token.token_type == *t
    }

    fn expect_peek(&mut self, t: &TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
            "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();

        if let ast::Node::Program(statements) = program {
            assert_eq!(statements.len(), 3);

            let expected = vec!["x", "y", "foobar"];
            for (i, t) in expected.into_iter().enumerate() {
                let stmt = statements.get(i).unwrap();
                match *stmt {
                    ast::Statement::Let(ref ident, _) => {
                        assert_eq!(ident.value, t);
                    }
                    _ => panic!(""),
                }
            }
        } else {
            panic!("Parse Error! {:?}", program);
        }
    }
}

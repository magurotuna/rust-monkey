use crate::ast;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use anyhow::{anyhow, Result};
use std::default::Default;
use std::mem;
use thiserror::Error;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<MonkeyParseError>,
}

type ExpectedTokenType = TokenType;
type ActualTokenType = TokenType;

#[derive(Error, Debug, PartialEq)]
enum MonkeyParseError {
    #[error("expected next token to be {expected:?}, got {actual:?} instead")]
    InvalidToken {
        expected: TokenType,
        actual: TokenType,
    },
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            cur_token: Token::default(),
            peek_token: Token::default(),
            errors: Vec::new(),
        };
        // Load 2 tokens to set both cur_token & peek_token
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.cur_token = mem::take(&mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Result<ast::Node> {
        let mut statements = Vec::new();
        while !self.cur_token_is(&TokenType::Eof) {
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
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
            self.peek_error(t);
            false
        }
    }

    fn peek_error(&mut self, expected_token_type: &TokenType) {
        let err = MonkeyParseError::InvalidToken {
            expected: *expected_token_type,
            actual: self.peek_token.token_type,
        };
        self.errors.push(err);
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
        check_parser_errors(&parser);

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

    #[test]
    fn test_let_statements_error() {
        let input = r#"
let x  5;
let = 10;
let 838383;
            "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parser_errors(&parser);
        assert_eq!(parser.errors.len(), 3);

        let expected_errors = [
            MonkeyParseError::InvalidToken {
                expected: TokenType::Assign,
                actual: TokenType::Int,
            },
            MonkeyParseError::InvalidToken {
                expected: TokenType::Identifier,
                actual: TokenType::Assign,
            },
            MonkeyParseError::InvalidToken {
                expected: TokenType::Identifier,
                actual: TokenType::Int,
            },
        ];
        parser
            .errors
            .iter()
            .zip(expected_errors.iter())
            .for_each(|(actual, expected)| assert_eq!(actual, expected));
    }

    fn check_parser_errors(parser: &Parser) -> bool {
        if parser.errors.len() == 0 {
            return false;
        }

        println!("parser has {} errors", parser.errors.len());
        parser
            .errors
            .iter()
            .for_each(|err| println!("parser error: {}", err));
        true
    }
}

use crate::ast;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use anyhow::{Error, Result};
use std::collections::HashMap;
use std::default::Default;
use std::mem;

lazy_static! {
    static ref PRECEDENCES: HashMap<TokenType, Precedence> = {
        let mut m = HashMap::new();
        m.insert(TokenType::Eq, Precedence::Equals);
        m.insert(TokenType::NotEq, Precedence::Equals);
        m.insert(TokenType::Lt, Precedence::LessGreater);
        m.insert(TokenType::Gt, Precedence::LessGreater);
        m.insert(TokenType::Plus, Precedence::Sum);
        m.insert(TokenType::Minus, Precedence::Sum);
        m.insert(TokenType::Slash, Precedence::Product);
        m.insert(TokenType::Asterisk, Precedence::Product);
        m
    };
}

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<Error>,
}

#[derive(thiserror::Error, Debug, PartialEq)]
enum MonkeyParseError {
    #[error("expected next token to be `{expected:?}`, got `{actual:?}` instead")]
    InvalidToken {
        expected: TokenType,
        actual: TokenType,
    },
    #[error("unable to parse an integer literal, got `{0}`")]
    UnableToParseIntegerLiteral(String),
    #[error("prefix parse function for `{0:?}` not found")]
    NoPrefixParseFn(TokenType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // < or >
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // func()
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

    fn parse_program(&mut self) -> Result<ast::Program> {
        let mut statements = Vec::new();
        while !self.cur_token_is(&TokenType::Eof) {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => self.errors.push(e),
            }
            self.next_token();
        }
        Ok(ast::Program(statements))
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement> {
        if !self.expect_peek(&TokenType::Identifier) {
            let err = MonkeyParseError::InvalidToken {
                expected: TokenType::Identifier,
                actual: self.peek_token.token_type,
            };
            return Err(Error::from(Box::new(err)));
        }

        let identifier = ast::Identifier(self.cur_token.literal.clone()); // cannot get rid of `clone`?

        if !self.expect_peek(&TokenType::Assign) {
            let err = MonkeyParseError::InvalidToken {
                expected: TokenType::Assign,
                actual: self.peek_token.token_type,
            };
            return Err(Error::from(Box::new(err)));
        }

        // TODO: skip to read the expression until encountering semicolon.
        while !self.cur_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ast::Statement::Let(identifier, ast::Expression::Dummy)) // FIXME
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        self.next_token();

        // TODO: skip to read the expression until encountering semicolon.
        while !self.cur_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ast::Statement::Return(ast::Expression::Dummy)) // FIXME
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        let statement = ast::Statement::ExpressionStatement(expr);
        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }
        Ok(statement)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression> {
        let mut left_expr = match self.cur_token.token_type {
            TokenType::Identifier => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            x => {
                let err = MonkeyParseError::NoPrefixParseFn(x);
                Err(Error::from(Box::new(err)))
            }
        }?;

        while !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence() {
            match self.peek_token.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterisk
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Lt
                | TokenType::Gt => {
                    self.next_token();
                    left_expr = self.parse_infix_expression(Box::new(left_expr))?;
                }
                _ => break,
            }
        }
        Ok(left_expr)
    }

    fn parse_identifier(&self) -> Result<ast::Expression> {
        Ok(ast::Expression::Identifier(ast::Identifier(
            self.cur_token.literal.clone(),
        )))
    }

    fn parse_integer_literal(&self) -> Result<ast::Expression> {
        let val: i64 = self.cur_token.literal.parse().map_err(|_| {
            MonkeyParseError::UnableToParseIntegerLiteral(self.cur_token.literal.clone())
        })?;
        Ok(ast::Expression::IntegerLiteral(val))
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(ast::Expression::Prefix {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parse_infix_expression(&mut self, left: Box<ast::Expression>) -> Result<ast::Expression> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(ast::Expression::Infix {
            token,
            operator,
            left,
            right: Box::new(right),
        })
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
        self.errors.push(Error::from(Box::new(err)));
    }

    fn cur_precedence(&self) -> Precedence {
        match (*PRECEDENCES).get(&self.cur_token.token_type) {
            Some(&p) => p,
            None => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        match (*PRECEDENCES).get(&self.peek_token.token_type) {
            Some(&p) => p,
            None => Precedence::Lowest,
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

        let ast::Program(statements) = parser.parse_program().unwrap();
        check_parser_errors(&parser);

        assert_eq!(statements.len(), 3);

        let expected = vec!["x", "y", "foobar"];
        for (i, t) in expected.into_iter().enumerate() {
            let stmt = statements.get(i).unwrap();
            match *stmt {
                ast::Statement::Let(ref ident, _) => {
                    let ast::Identifier(value) = ident;
                    assert_eq!(value, t);
                }
                _ => panic!(""),
            }
        }
    }

    // TODO: uncomment out after implementing expression parser
    //#[test]
    //fn test_let_statements_error() {
    //let input = r#"
    //let x  5;
    //let = 10;
    //let 838383;
    //"#;
    //let lexer = Lexer::new(input.to_string());
    //let mut parser = Parser::new(lexer);

    //let program = parser.parse_program().unwrap();
    //check_parser_errors(&parser);
    //assert_eq!(parser.errors.len(), 3);

    //let expected_errors = [
    //MonkeyParseError::InvalidToken {
    //expected: TokenType::Assign,
    //actual: TokenType::Int,
    //},
    //MonkeyParseError::InvalidToken {
    //expected: TokenType::Identifier,
    //actual: TokenType::Assign,
    //},
    //MonkeyParseError::InvalidToken {
    //expected: TokenType::Identifier,
    //actual: TokenType::Int,
    //},
    //];
    //parser
    //.errors
    //.into_iter()
    //.zip(expected_errors.into_iter())
    //.map(|(actual, expected)| (format!("{}", actual), format!("{}", expected)))
    //.for_each(|(actual_msg, expected_msg)| {
    //assert_eq!(actual_msg, expected_msg);
    //});
    //}

    #[test]
    fn test_return_statements() {
        let input = r#"
return 5;
return 10;
return 993322;
            "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let ast::Program(statements) = parser.parse_program().unwrap();
        check_parser_errors(&parser);

        assert_eq!(statements.len(), 3);

        statements.iter().for_each(|stmt| match *stmt {
            ast::Statement::Return(_) => (),
            _ => panic!("the statement expected to be `return`, got {:?}", stmt),
        });
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let ast::Program(statements) = parser.parse_program().unwrap();
        check_parser_errors(&parser);

        assert_eq!(statements.len(), 1);

        use ast::{Expression, Identifier, Statement};
        if let Some(Statement::ExpressionStatement(Expression::Identifier(Identifier(ref ident)))) =
            statements.get(0)
        {
            assert_eq!(ident, "foobar");
        } else {
            panic!(
                "identifier cannot be parsed properly. statement: {}",
                statements.get(0).unwrap()
            );
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let ast::Program(statements) = parser.parse_program().unwrap();
        check_parser_errors(&parser);

        assert_eq!(statements.len(), 1);

        use ast::{Expression, Identifier, Statement};
        if let Some(Statement::ExpressionStatement(Expression::IntegerLiteral(ref val))) =
            statements.get(0)
        {
            assert_eq!(val, &5);
        } else {
            panic!(
                "integer literal cannot be parsed properly. statement: {}",
                statements.get(0).unwrap()
            );
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct PrefixTest {
            input: &'static str,
            operator: &'static str,
            int_value: i64,
        };
        let prefix_tests = [
            PrefixTest {
                input: "!5;",
                operator: "!",
                int_value: 5,
            },
            PrefixTest {
                input: "-15;",
                operator: "-",
                int_value: 15,
            },
        ];

        for test in prefix_tests.iter() {
            let lexer = Lexer::new(test.input.to_string());
            let mut parser = Parser::new(lexer);

            let ast::Program(statements) = parser.parse_program().unwrap();
            check_parser_errors(&parser);

            assert_eq!(statements.len(), 1);

            use ast::{Expression, Identifier, Statement};
            if let Some(Statement::ExpressionStatement(Expression::Prefix {
                ref operator,
                ref right,
                ..
            })) = statements.get(0)
            {
                assert_eq!(operator, test.operator);
                test_integer_literal(right, test.int_value);
            } else {
                panic!(
                    "prefix expression cannot be parsed properly. statement: {}",
                    statements.get(0).unwrap()
                );
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct InfixTest {
            input: &'static str,
            left_value: i64,
            operator: &'static str,
            right_value: i64,
        };
        impl InfixTest {
            fn new(
                input: &'static str,
                left_value: i64,
                operator: &'static str,
                right_value: i64,
            ) -> Self {
                InfixTest {
                    input,
                    left_value,
                    operator,
                    right_value,
                }
            }
        }
        let infix_tests = [
            InfixTest::new("5 + 2;", 5, "+", 2),
            InfixTest::new("5 - 2;", 5, "-", 2),
            InfixTest::new("5 * 2;", 5, "*", 2),
            InfixTest::new("5 / 2;", 5, "/", 2),
            InfixTest::new("5 > 2;", 5, ">", 2),
            InfixTest::new("5 < 2;", 5, "<", 2),
            InfixTest::new("5 == 2;", 5, "==", 2),
            InfixTest::new("5 != 2;", 5, "!=", 2),
        ];

        for test in infix_tests.iter() {
            let lexer = Lexer::new(test.input.to_string());
            let mut parser = Parser::new(lexer);

            let ast::Program(statements) = parser.parse_program().unwrap();
            check_parser_errors(&parser);

            assert_eq!(statements.len(), 1);

            use ast::{Expression, Identifier, Statement};
            if let Some(Statement::ExpressionStatement(Expression::Infix {
                ref operator,
                ref right,
                ref left,
                ..
            })) = statements.get(0)
            {
                test_integer_literal(left, test.left_value);
                assert_eq!(operator, test.operator);
                test_integer_literal(right, test.right_value);
            } else {
                panic!(
                    "infix expression cannot be parsed properly. statement: {}",
                    statements.get(0).unwrap()
                );
            }
        }
    }

    fn test_integer_literal(il: &ast::Expression, value: i64) {
        if let ast::Expression::IntegerLiteral(ref int) = il {
            assert_eq!(int, &value);
        } else {
            panic!(
                "given expression `{}` expected to be IntegerLiteral, but NOT",
                &il
            );
        }
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

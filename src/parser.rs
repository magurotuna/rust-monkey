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
        m.insert(TokenType::LParen, Precedence::Call);
        m
    };
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: MonkeyParseErrors,
}

#[derive(Debug, Error)]
#[error("{}", .0.iter().map(|e| format!("\t{}", e)).collect::<Vec<_>>().join("\n"))]
pub struct MonkeyParseErrors(Vec<Error>);

#[derive(thiserror::Error, Debug, PartialEq)]
enum MonkeyParseError {
    #[error("expected next token to be `{expected:?}`, got `{actual:?}` instead")]
    InvalidToken {
        expected: TokenType,
        actual: TokenType,
    },
    #[error("unable to parse an integer literal, got `{0}`")]
    UnableToParseIntegerLiteral(String),
    #[error("unable to parse an boolean literal, got `{0}`")]
    UnableToParseBooleanLiteral(String),
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
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            cur_token: Token::default(),
            peek_token: Token::default(),
            errors: MonkeyParseErrors(Vec::new()),
        };
        // Load 2 tokens to set both cur_token & peek_token
        p.next_token();
        p.next_token();
        p
    }

    /// NOTE: This function consumes Parser.
    pub fn parse_program(mut self) -> Result<ast::Program> {
        let mut statements = Vec::new();
        while !self.cur_token_is(&TokenType::Eof) {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    let MonkeyParseErrors(ref mut errors) = self.errors;
                    errors.push(e);
                }
            }
            self.next_token();
        }

        let MonkeyParseErrors(ref errors) = self.errors;
        if !errors.is_empty() {
            return Err(Error::from(Box::new(self.errors)));
        }
        Ok(ast::Program(statements))
    }

    fn next_token(&mut self) {
        self.cur_token = mem::take(&mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement> {
        self.expect_peek(&TokenType::Identifier)?;

        let identifier = ast::Identifier(self.cur_token.literal.clone()); // cannot get rid of `clone`?

        self.expect_peek(&TokenType::Assign)?;

        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ast::Statement::Let(identifier, value))
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ast::Statement::Return(expr))
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
            TokenType::True | TokenType::False => self.parse_boolean_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(),
            TokenType::Function => self.parse_function_literal(),
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
                TokenType::LParen => {
                    self.next_token();
                    left_expr = self.parse_call_expression(Box::new(left_expr))?;
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

    fn parse_boolean_literal(&self) -> Result<ast::Expression> {
        match self.cur_token.token_type {
            TokenType::True | TokenType::False => Ok(ast::Expression::BooleanLiteral(
                self.cur_token_is(&TokenType::True),
            )),
            _ => Err(Error::from(Box::new(
                MonkeyParseError::UnableToParseBooleanLiteral(self.cur_token.literal.clone()),
            ))),
        }
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

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression> {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(&TokenType::RParen)?;
        Ok(expr)
    }

    fn parse_if_expression(&mut self) -> Result<ast::Expression> {
        let token = self.cur_token.clone();
        self.expect_peek(&TokenType::LParen)?;
        self.next_token();
        let condition_expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(&TokenType::RParen)?;
        self.expect_peek(&TokenType::LBrace)?;
        let consequence_stmts = self.parse_block_statement()?;

        let alternative_stmts = if self.peek_token_is(&TokenType::Else) {
            self.next_token();
            self.expect_peek(&TokenType::LBrace)?;
            let stmts = self.parse_block_statement()?;
            Some(stmts)
        } else {
            None
        };

        Ok(ast::Expression::If {
            token,
            condition: Box::new(condition_expr),
            consequence: consequence_stmts,
            alternative: alternative_stmts,
        })
    }

    fn parse_function_literal(&mut self) -> Result<ast::Expression> {
        let token = self.cur_token.clone();
        self.expect_peek(&TokenType::LParen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(&TokenType::LBrace)?;
        let body = self.parse_block_statement()?;
        Ok(ast::Expression::FunctionLiteral {
            token,
            parameters,
            body,
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

    fn parse_call_expression(&mut self, left: Box<ast::Expression>) -> Result<ast::Expression> {
        let token = self.cur_token.clone();
        let args = self.parse_call_arguments()?;
        Ok(ast::Expression::Call {
            token,
            function: left,
            arguments: args,
        })
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Box<ast::Expression>>> {
        let mut args = Vec::new();

        // no arguments
        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(Box::new(self.parse_expression(Precedence::Lowest)?));

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            args.push(Box::new(self.parse_expression(Precedence::Lowest)?));
        }

        self.expect_peek(&TokenType::RParen)?;
        Ok(args)
    }

    fn parse_block_statement(&mut self) -> Result<ast::BlockStatement> {
        let mut stmts = Vec::new();
        self.next_token();

        while !self.cur_token_is(&TokenType::RBrace) && !self.cur_token_is(&TokenType::Eof) {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            self.next_token();
        }

        Ok(ast::BlockStatement(stmts))
    }

    fn parse_function_parameters(&mut self) -> Result<ast::FunctionParameters> {
        let mut identifiers = Vec::new();

        // no parameters
        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return Ok(ast::FunctionParameters(identifiers));
        }

        self.next_token();

        identifiers.push(ast::Identifier(self.cur_token.literal.clone()));

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            identifiers.push(ast::Identifier(self.cur_token.literal.clone()));
        }

        self.expect_peek(&TokenType::RParen)?;
        Ok(ast::FunctionParameters(identifiers))
    }

    fn cur_token_is(&self, t: &TokenType) -> bool {
        self.cur_token.token_type == *t
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        self.peek_token.token_type == *t
    }

    fn expect_peek(&mut self, t: &TokenType) -> Result<()> {
        if self.peek_token_is(t) {
            self.next_token();
            Ok(())
        } else {
            Err(self.peek_error(t))
        }
    }

    fn peek_error(&mut self, expected_token_type: &TokenType) -> Error {
        let err = MonkeyParseError::InvalidToken {
            expected: *expected_token_type,
            actual: self.peek_token.token_type,
        };
        Error::from(Box::new(err))
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
    use crate::token::{IntoToken, Token, TokenType};

    #[test]
    fn test_let_statements() {
        struct TestLet {
            input: &'static str,
            expected_identifier: &'static str,
            expected_value: Box<dyn IntoToken>,
        };
        let let_tests = [
            TestLet {
                input: "let x = 5;",
                expected_identifier: "x",
                expected_value: Box::new(5),
            },
            TestLet {
                input: "let y = true;",
                expected_identifier: "y",
                expected_value: Box::new(true),
            },
            TestLet {
                input: "let foobar = y;",
                expected_identifier: "foobar",
                expected_value: Box::new("y"),
            },
        ];

        for test in let_tests.iter() {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);

            let ast::Program(statements) = parser.parse_program().unwrap();
            check_parser_errors(&parser);

            use ast::{Expression, Identifier, Statement};
            assert_eq!(statements.len(), 1);
            if let Some(Statement::Let(Identifier(ref ident), ref expr)) = statements.get(0) {
                assert_eq!(test.expected_identifier, ident);
                test_literal_expression(expr, &test.expected_value.into_token());
            } else {
                panic!(
                    "let statement cannot be parsed properly, got {}",
                    statements.get(0).unwrap()
                );
            }
        }
    }

    // TODO: uncomment out after implementing expression parser
    #[test]
    fn test_let_statements_error() {
        let input = r#"
    let x  5;
    let 838384;
    "#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parser_errors(&parser);
        assert_eq!(parser.errors.len(), 2);

        let expected_errors = [
            MonkeyParseError::InvalidToken {
                expected: TokenType::Assign,
                actual: TokenType::Int,
            },
            MonkeyParseError::InvalidToken {
                expected: TokenType::Identifier,
                actual: TokenType::Int,
            },
        ];
        parser
            .errors
            .into_iter()
            .zip(expected_errors.into_iter())
            .map(|(actual, expected)| (format!("{}", actual), format!("{}", expected)))
            .for_each(|(actual_msg, expected_msg)| {
                assert_eq!(actual_msg, expected_msg);
            });
    }

    #[test]
    fn test_return_statements() {
        struct TestReturn {
            input: &'static str,
            expected_value: Box<dyn IntoToken>,
        };
        let let_tests = [
            TestReturn {
                input: "return 5;",
                expected_value: Box::new(5),
            },
            TestReturn {
                input: "return true;",
                expected_value: Box::new(true),
            },
            TestReturn {
                input: "return foobar;",
                expected_value: Box::new("foobar"),
            },
        ];

        for test in let_tests.iter() {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);

            let ast::Program(statements) = parser.parse_program().unwrap();
            check_parser_errors(&parser);

            use ast::{Expression, Identifier, Statement};
            assert_eq!(statements.len(), 1);
            if let Some(Statement::Return(ref expr)) = statements.get(0) {
                test_literal_expression(expr, &test.expected_value.into_token());
            } else {
                panic!(
                    "return statement cannot be parsed properly, got {}",
                    statements.get(0).unwrap()
                );
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input);
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

        let lexer = Lexer::new(input);
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
    fn test_boolean_literal_expression() {
        #[derive(new)]
        struct BooleanTest {
            input: &'static str,
            expected: bool,
        };
        let boolean_test = [
            BooleanTest::new("true;", true),
            BooleanTest::new("false;", false),
        ];

        for test in boolean_test.iter() {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);

            let ast::Program(statements) = parser.parse_program().unwrap();
            check_parser_errors(&parser);

            assert_eq!(statements.len(), 1);

            use ast::{Expression, Identifier, Statement};
            if let Some(Statement::ExpressionStatement(Expression::BooleanLiteral(ref val))) =
                statements.get(0)
            {
                assert_eq!(&test.expected, val);
            } else {
                panic!(
                    "boolean literal cannot be parsed properly. statement: {}",
                    statements.get(0).unwrap()
                );
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        #[derive(new)]
        struct PrefixTest {
            input: &'static str,
            operator: &'static str,
            int_value: i64,
        };
        let prefix_tests = [
            PrefixTest::new("!5;", "!", 5),
            PrefixTest::new("-15;", "-", 15),
        ];

        for test in prefix_tests.iter() {
            let lexer = Lexer::new(test.input);
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
    fn test_parsing_infix_integer_expressions() {
        struct InfixTest {
            input: &'static str,
            left_value: Box<dyn IntoToken>,
            operator: &'static str,
            right_value: Box<dyn IntoToken>,
        }

        impl InfixTest {
            fn new(
                input: &'static str,
                left_value: impl IntoToken + 'static,
                operator: &'static str,
                right_value: impl IntoToken + 'static,
            ) -> Self {
                InfixTest {
                    input,
                    operator,
                    left_value: Box::new(left_value),
                    right_value: Box::new(right_value),
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
            InfixTest::new("alice != bob;", "alice", "!=", "bob"),
            InfixTest::new("foo * bar;", "foo", "*", "bar"),
            InfixTest::new("hoge < piyo", "hoge", "<", "piyo"),
            InfixTest::new("true != false;", true, "!=", false),
            InfixTest::new("true * false;", true, "*", false),
            InfixTest::new("false < true", false, "<", true),
        ];
        for infix_test in infix_tests.into_iter() {
            let lexer = Lexer::new(infix_test.input);
            let mut parser = Parser::new(lexer);

            let ast::Program(statements) = parser.parse_program().unwrap();
            check_parser_errors(&parser);

            assert_eq!(statements.len(), 1);

            if let Some(ast::Statement::ExpressionStatement(ref expr)) = statements.get(0) {
                test_infix_expression(
                    expr,
                    &infix_test.left_value,
                    infix_test.operator,
                    &infix_test.right_value,
                );
            } else {
                panic!(
                    "infix expression cannot be parsed properly. statement: {}",
                    statements.get(0).unwrap()
                );
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        #[derive(new)]
        struct PrecedenceTest {
            input: &'static str,
            expected: &'static str,
        };

        let precedence_tests = [
            PrecedenceTest::new("-a * b", "((-a) * b)"),
            PrecedenceTest::new("!-a", "(!(-a))"),
            PrecedenceTest::new("a + b + c", "((a + b) + c)"),
            PrecedenceTest::new("a + b - c", "((a + b) - c)"),
            PrecedenceTest::new("a * b * c", "((a * b) * c)"),
            PrecedenceTest::new("a * b / c", "((a * b) / c)"),
            PrecedenceTest::new("a + b / c", "(a + (b / c))"),
            PrecedenceTest::new("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            PrecedenceTest::new("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            PrecedenceTest::new("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            PrecedenceTest::new("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            PrecedenceTest::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            PrecedenceTest::new("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            PrecedenceTest::new("(5 + 5) * 2", "((5 + 5) * 2)"),
            PrecedenceTest::new("2 / (5 + 5)", "(2 / (5 + 5))"),
            PrecedenceTest::new("-(5 + 5)", "(-(5 + 5))"),
            PrecedenceTest::new("!(true == true)", "(!(true == true))"),
            PrecedenceTest::new("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            PrecedenceTest::new(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            PrecedenceTest::new(
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for test in precedence_tests.iter() {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap();
            check_parser_errors(&parser);
            assert_eq!(&format!("{}", program), test.expected);
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let ast::Program(statements) = parser.parse_program().unwrap();
        check_parser_errors(&parser);
        assert_eq!(statements.len(), 1);

        use ast::{BlockStatement, Expression, FunctionParameters, Identifier, Statement};
        if let Some(Statement::ExpressionStatement(Expression::FunctionLiteral {
            parameters: FunctionParameters(ref parameters),
            ref body,
            ..
        })) = statements.get(0)
        {
            // parameters assertion
            assert_eq!(parameters.len(), 2);
            assert_eq!(parameters.get(0), Some(&Identifier("x".to_string())));
            assert_eq!(parameters.get(1), Some(&Identifier("y".to_string())));

            // function body assertion
            let &BlockStatement(ref body_stmts) = body;
            assert_eq!(body_stmts.len(), 1);
            if let Some(Statement::ExpressionStatement(ref expr)) = body_stmts.get(0) {
                test_infix_expression(expr, &"x", "+", &"y");
            } else {
                panic!(
                    "function body block parse error. first statement: `{}`",
                    body_stmts.get(0).unwrap()
                );
            }
        } else {
            panic!(
                "prefix expression cannot be parsed properly. statement: {}",
                statements.get(0).unwrap()
            );
        }
    }

    #[test]
    fn test_function_parameters_parsing() {
        #[derive(new)]
        struct FnParamTest {
            input: &'static str,
            expected_params: Vec<&'static str>,
        };
        let tests = [
            FnParamTest::new("fn() {}", vec![]),
            FnParamTest::new("fn(x) {}", vec!["x"]),
            FnParamTest::new("fn(x, y) {}", vec!["x", "y"]),
            FnParamTest::new("fn(foo, bar, baz) {}", vec!["foo", "bar", "baz"]),
            FnParamTest::new(
                "fn(foo, bar, baz) { foo * bar - baz }",
                vec!["foo", "bar", "baz"],
            ),
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);

            use ast::{Expression, FunctionParameters, Identifier, Program, Statement};
            let Program(statements) = parser.parse_program().unwrap();
            check_parser_errors(&parser);
            assert_eq!(statements.len(), 1);

            if let Some(Statement::ExpressionStatement(Expression::FunctionLiteral {
                parameters: FunctionParameters(ref params),
                ..
            })) = statements.get(0)
            {
                params
                    .iter()
                    .enumerate()
                    .for_each(|(i, Identifier(ref ident))| {
                        assert_eq!(test.expected_params[i], ident)
                    });
            } else {
                panic!(
                    "function literal cannot be parsed properly. statement: {}",
                    statements.get(0).unwrap()
                );
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        use ast::{Expression, FunctionParameters, Identifier, Program, Statement};
        let Program(statements) = parser.parse_program().unwrap();
        check_parser_errors(&parser);
        assert_eq!(statements.len(), 1);

        if let Some(Statement::ExpressionStatement(Expression::Call {
            ref function,
            ref arguments,
            ..
        })) = statements.get(0)
        {
            test_identifier(function, "add");
            assert_eq!(arguments.len(), 3);
            test_literal_expression(&arguments[0], &1.into_token());
            test_infix_expression(&arguments[1], &2, "*", &3);
            test_infix_expression(&arguments[2], &4, "+", &5);
        } else {
            panic!(
                "call expression cannot be parsed properly. statement: {}",
                statements.get(0).unwrap()
            );
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let ast::Program(statements) = parser.parse_program().unwrap();
        check_parser_errors(&parser);
        assert_eq!(statements.len(), 1);

        use ast::{BlockStatement, Expression, Identifier, Statement};
        if let Some(Statement::ExpressionStatement(Expression::If {
            ref condition,
            ref consequence,
            ref alternative,
            ..
        })) = statements.get(0)
        {
            // condition assertion
            test_infix_expression(condition, &"x", "<", &"y");

            // consequence assertion
            let &BlockStatement(ref cons_stmts) = consequence;
            assert_eq!(1, cons_stmts.len());
            if let Some(Statement::ExpressionStatement(ref expr)) = cons_stmts.get(0) {
                test_identifier(expr, "x");
            } else {
                panic!(
                    "consequence block parse error. first statement: `{}`",
                    cons_stmts.get(0).unwrap()
                );
            }

            // alternative assertion
            assert!(alternative.is_none());
        } else {
            panic!(
                "prefix expression cannot be parsed properly. statement: {}",
                statements.get(0).unwrap()
            );
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let ast::Program(statements) = parser.parse_program().unwrap();
        check_parser_errors(&parser);
        assert_eq!(statements.len(), 1);

        use ast::{BlockStatement, Expression, Identifier, Statement};
        if let Some(Statement::ExpressionStatement(Expression::If {
            ref condition,
            ref consequence,
            ref alternative,
            ..
        })) = statements.get(0)
        {
            // condition assertion
            test_infix_expression(condition, &"x", "<", &"y");

            // consequence assertion
            let &BlockStatement(ref cons_stmts) = consequence;
            assert_eq!(1, cons_stmts.len());
            if let Some(Statement::ExpressionStatement(ref expr)) = cons_stmts.get(0) {
                test_identifier(expr, "x");
            } else {
                panic!(
                    "consequence block parse error. first statement: `{}`",
                    cons_stmts.get(0).unwrap()
                );
            }

            // alternative assertion
            let BlockStatement(ref alt_stmts) = alternative.as_ref().unwrap();
            assert_eq!(1, cons_stmts.len());
            if let Some(Statement::ExpressionStatement(ref expr)) = alt_stmts.get(0) {
                test_identifier(expr, "y");
            } else {
                panic!(
                    "alternative block parse error. first statement: `{}`",
                    alt_stmts.get(0).unwrap()
                );
            }
        } else {
            panic!(
                "prefix expression cannot be parsed properly. statement: {}",
                statements.get(0).unwrap()
            );
        }
    }

    fn test_integer_literal(il: &ast::Expression, value: i64) {
        if let ast::Expression::IntegerLiteral(ref int) = il {
            assert_eq!(int, &value);
        } else {
            panic!(
                "given expression `{}` expected to be IntegerLiteral, but NOT",
                il
            );
        }
    }

    fn test_identifier(expr: &ast::Expression, value: &str) {
        if let ast::Expression::Identifier(ast::Identifier(ref ident)) = expr {
            assert_eq!(ident, value);
        } else {
            panic!(
                "given expression `{}` expected to be Identifier, but NOT",
                expr
            );
        }
    }

    fn test_boolean_literal(expr: &ast::Expression, value: &bool) {
        if let ast::Expression::BooleanLiteral(b) = expr {
            assert_eq!(b, value);
        } else {
            panic!(
                "given expression `{}` expected to be BooleanLiteral, but NOT",
                expr
            );
        }
    }

    fn test_literal_expression(expr: &ast::Expression, expected: &Token) {
        match expected.token_type {
            TokenType::Identifier => test_identifier(expr, expected.literal.as_str()),
            TokenType::Int => test_integer_literal(expr, expected.literal.parse().unwrap()),
            TokenType::True | TokenType::False => {
                test_boolean_literal(expr, &expected.literal.parse().unwrap())
            }
            _ => panic!("type of exp not handled. got={}", expr),
        }
    }

    fn test_infix_expression(
        expr: &ast::Expression,
        expected_left: &dyn IntoToken,
        expected_operator: &str,
        expected_right: &dyn IntoToken,
    ) {
        if let &ast::Expression::Infix {
            ref operator,
            ref right,
            ref left,
            ..
        } = expr
        {
            assert_eq!(expected_operator, operator);
            test_literal_expression(left, &expected_left.into_token());
            test_literal_expression(right, &expected_right.into_token());
        } else {
            panic!("expr is not Expression. got={}", expr);
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

use crate::token::Token;
use std::fmt;

#[derive(Debug)]
pub struct Program(pub Vec<Statement>);

#[derive(Debug)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(i64),
    Dummy,
}

#[derive(Debug)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    ExpressionStatement(Expression),
}

#[derive(Debug)]
pub struct Identifier(pub String);

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Program(ref stmts) = self;
        write!(
            f,
            "{}",
            stmts
                .iter()
                .map(|stmt| format!("{}", stmt))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Node::Statement(ref stmt) => write!(f, "{}", stmt),
            Node::Expression(ref expr) => write!(f, "{}", expr),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Expression::Identifier(ref ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(ref val) => write!(f, "{}", val),
            Expression::Dummy => write!(f, "THIS SHOULD BE FIXED"), // FIXME
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Statement::Let(Identifier(ref ident), ref expr) => {
                write!(f, "let {} = {};", ident, expr)
            }
            Statement::Return(ref expr) => write!(f, "return {};", expr),
            Statement::ExpressionStatement(ref expr) => write!(f, "{}", expr),
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Identifier(ref ident) = self;
        write!(f, "{}", ident)
    }
}

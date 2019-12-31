use crate::token::Token;

pub type Program = Vec<Statement>;

#[derive(Debug)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
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

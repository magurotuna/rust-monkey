use crate::token::Token;

#[derive(Debug)]
pub enum Node {
    Program(Vec<Statement>),
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
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        match self.statements.len() {
            0 => "",
            _ => self.statements[0].token_literal(),
        }
    }
}

struct LetStatement<T: Expression> {
    token: Token,
    name: Identifier,
    value: T,
}

impl<T> Statement for LetStatement<T>
where
    T: Expression,
{
    fn statement_node(&self) {
        unimplemented!();
    }
}

impl<T> Node for LetStatement<T>
where
    T: Expression,
{
    fn token_literal(&self) -> &str {
        self.token.literal.as_str()
    }
}

struct Identifier {
    token: Token,
    value: String,
}

impl Expression for Identifier {
    fn expression_node(&self) {
        unimplemented!();
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        self.token.literal.as_str()
    }
}

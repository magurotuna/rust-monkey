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

pub struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        self.statements
            .get(0)
            .map(|s| s.token_literal())
            .unwrap_or("")
    }
}

pub struct LetStatement<T: Expression> {
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

pub struct Identifier {
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

use crate::token::Token;
use std::fmt;

//#[derive(Debug)]
//pub struct Program(pub Vec<Statement>);

#[derive(Debug)]
pub enum Node {
    Program(Vec<Node>),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    Prefix {
        token: Token,
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        token: Token,
        operator: String,
        right: Box<Expression>,
        left: Box<Expression>,
    },
    If {
        token: Token,
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FunctionLiteral {
        token: Token,
        parameters: FunctionParameters,
        body: BlockStatement,
    },
    Call {
        token: Token,
        function: Box<Expression>,
        arguments: Vec<Box<Expression>>,
    },
}

#[derive(Debug)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    ExpressionStatement(Expression),
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub struct BlockStatement(pub Vec<Statement>);

#[derive(Debug)]
pub struct FunctionParameters(pub Vec<Identifier>);

//impl fmt::Display for Program {
//fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//let &Program(ref stmts) = self;
//write!(
//f,
//"{}",
//stmts
//.iter()
//.map(|stmt| format!("{}", stmt))
//.collect::<Vec<_>>()
//.join("")
//)
//}
//}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Node::Program(ref nodes) => write!(
                f,
                "{}",
                nodes
                    .iter()
                    .map(|n| format!("{}", n))
                    .collect::<Vec<_>>()
                    .join("")
            ),
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
            Expression::BooleanLiteral(ref val) => write!(f, "{}", val),
            Expression::Prefix {
                ref operator,
                ref right,
                ..
            } => write!(f, "({}{})", operator, right),
            Expression::Infix {
                ref operator,
                ref right,
                ref left,
                ..
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::If {
                ref condition,
                ref consequence,
                ref alternative,
                ..
            } => write!(
                f,
                "if{} {}{}",
                condition,
                consequence,
                match *alternative {
                    Some(ref alt) => format!("else {}", alt),
                    None => "".to_string(),
                }
            ),
            Expression::FunctionLiteral {
                parameters: FunctionParameters(ref parameters),
                ref body,
                ..
            } => write!(
                f,
                "fn({}) {}",
                parameters
                    .iter()
                    .map(|p| {
                        let Identifier(ref ident) = p;
                        ident.to_string()
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
                body,
            ),
            Expression::Call {
                ref function,
                ref arguments,
                ..
            } => write!(
                f,
                "{}({})",
                function,
                arguments
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
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

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &BlockStatement(ref stmts) = self;
        write!(
            f,
            "{}",
            stmts
                .iter()
                .map(|s| format!("{}", s))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

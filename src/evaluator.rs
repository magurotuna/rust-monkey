use crate::ast::{BlockStatement, Expression, FunctionParameters, Identifier, Node, Statement};
use crate::object::Object;

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(nodes) => nodes
            .into_iter()
            .fold(None, |_, node| Some(eval(node)))
            .unwrap_or(Object::Null), // TODO: Is Object::Null OK?
        Node::Statement(ref stmt) => eval_statement(stmt),
        Node::Expression(ref expr) => eval_expression(expr),
    }
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Let(Identifier(ident), expr) => todo!(),
        Statement::Return(expr) => eval_expression(expr),
        Statement::ExpressionStatement(expr) => eval_expression(expr),
    }
}

fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::Identifier(Identifier(ident)) => todo!(),
        Expression::IntegerLiteral(value) => Object::Integer(*value),
        Expression::BooleanLiteral(value) => Object::Boolean(*value),
        Expression::Prefix {
            operator, right, ..
        } => {
            let evaluated_right = eval_expression(right);
            eval_prefix_expression(operator, evaluated_right)
        }
        Expression::Infix {
            operator,
            left,
            right,
            ..
        } => {
            let evaluated_left = eval_expression(left);
            let evaluated_right = eval_expression(right);
            eval_infix_expression(operator, evaluated_left, evaluated_right)
        }
        _ => todo!(),
    }
    //Prefix {
    //token: Token,
    //operator: String,
    //right: Box<Expression>,
    //},
    //Infix {
    //token: Token,
    //operator: String,
    //right: Box<Expression>,
    //left: Box<Expression>,
    //},
    //If {
    //token: Token,
    //condition: Box<Expression>,
    //consequence: BlockStatement,
    //alternative: Option<BlockStatement>,
    //},
    //FunctionLiteral {
    //token: Token,
    //parameters: FunctionParameters,
    //body: BlockStatement,
    //},
    //Call {
    //token: Token,
    //function: Box<Expression>,
    //arguments: Vec<Box<Expression>>,
    //},
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_operator_expression(right),
        _ => Object::Null,
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-value),
        _ => Object::Null,
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left_val), Object::Integer(right_val)) => {
            eval_integer_infix_expression(operator, left_val, right_val)
        }
        _ => Object::Null,
    }
}

fn eval_integer_infix_expression(operator: &str, left_val: i64, right_val: i64) -> Object {
    match operator {
        "+" => Object::Integer(left_val + right_val),
        "-" => Object::Integer(left_val - right_val),
        "*" => Object::Integer(left_val * right_val),
        "/" => Object::Integer(left_val / right_val), // TODO: do check right_val != 0
        _ => Object::Null,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn test_eval_interger_expression() {
        #[derive(new)]
        struct TestInteger {
            input: &'static str,
            expected: i64,
        };
        let tests = [
            TestInteger::new("5", 5),
            TestInteger::new("10", 10),
            TestInteger::new("-5", -5),
            TestInteger::new("-10", -10),
            TestInteger::new("5 + 5 + 5 + 5 - 10", 10),
            TestInteger::new("2 * 2 * 2 * 2 * 2", 32),
            TestInteger::new("-50 + 100 - 50", 0),
            TestInteger::new("5 * 2 + 10", 20),
            TestInteger::new("5 + 2 * 10", 25),
            TestInteger::new("20 + 2 * -10", 0),
            TestInteger::new("50 / 2 * 2 + 10", 60),
            TestInteger::new("2 * (5 + 10)", 30),
            TestInteger::new("3 * 3 * 3 + 10", 37),
            TestInteger::new("3 * (3 * 3) + 10", 37),
            TestInteger::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input);
            test_integer_object(evaluated, test.expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        #[derive(new)]
        struct TestBoolean {
            input: &'static str,
            expected: bool,
        };
        let tests = [
            TestBoolean::new("true", true),
            TestBoolean::new("false", false),
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input);
            test_boolean_object(evaluated, test.expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        #[derive(new)]
        struct TestBang {
            input: &'static str,
            expected: bool,
        };
        let tests = [
            TestBang::new("!true", false),
            TestBang::new("!false", true),
            TestBang::new("!5", false),
            TestBang::new("!!true", true),
            TestBang::new("!!false", false),
            TestBang::new("!!5", true),
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input);
            test_boolean_object(evaluated, test.expected);
        }
    }

    fn test_eval(input: impl AsRef<str>) -> Object {
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().expect("parse error!");
        eval(program)
    }

    fn test_integer_object(evaluated: Object, expected: i64) {
        if let Object::Integer(value) = evaluated {
            assert_eq!(expected, value);
        } else {
            panic!(
                "should be evaluated as integer literal, but got {}",
                evaluated
            );
        }
    }

    fn test_boolean_object(evaluated: Object, expected: bool) {
        if let Object::Boolean(value) = evaluated {
            assert_eq!(expected, value);
        } else {
            panic!(
                "should be evaluated as boolean literal, but got {}",
                evaluated
            );
        }
    }
}

use crate::token::{self, Token, TokenType};
use std::default::Default;

#[derive(Default, Debug)]
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input: input.chars().collect(),
            ..Default::default()
        };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Eq, "==".to_string())
                } else {
                    Token::new(TokenType::Assign, self.ch.to_string())
                }
            }
            ';' => Token::new(TokenType::Semicolon, self.ch.to_string()),
            '(' => Token::new(TokenType::LParen, self.ch.to_string()),
            ')' => Token::new(TokenType::RParen, self.ch.to_string()),
            ',' => Token::new(TokenType::Comma, self.ch.to_string()),
            '+' => Token::new(TokenType::Plus, self.ch.to_string()),
            '-' => Token::new(TokenType::Minus, self.ch.to_string()),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".to_string())
                } else {
                    Token::new(TokenType::Bang, self.ch.to_string())
                }
            }
            '/' => Token::new(TokenType::Slash, self.ch.to_string()),
            '*' => Token::new(TokenType::Asterisk, self.ch.to_string()),
            '<' => Token::new(TokenType::Lt, self.ch.to_string()),
            '>' => Token::new(TokenType::Gt, self.ch.to_string()),
            '{' => Token::new(TokenType::LBrace, self.ch.to_string()),
            '}' => Token::new(TokenType::RBrace, self.ch.to_string()),
            c if (c as u8) == 0 => Token::new(TokenType::Eof, "".to_string()),
            _ => {
                if is_letter(self.ch) {
                    match self.read_identifier() {
                        Some(literal) => Token::new(token::lookup_identifier(&literal), literal),
                        None => Token::new(TokenType::Illegal, self.ch.to_string()), // FIXME... Is this necessary?
                    }
                } else if is_digit(self.ch) {
                    match self.read_number() {
                        Some(num) => Token::new(TokenType::Int, num),
                        None => Token::new(TokenType::Illegal, self.ch.to_string()), // FIXME... Is this necessary?
                    }
                } else {
                    Token::new(TokenType::Illegal, self.ch.to_string())
                }
            }
        };
        if token.is_identifier_or_keywords() {
            return token;
        }
        self.read_char();
        token
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            0 as char
        } else {
            self.input[self.read_position]
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0 as char; // null character
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> Option<String> {
        let start_position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        self.input
            .get(start_position..self.position)
            .map(|s| s.iter().collect::<String>())
    }

    fn read_number(&mut self) -> Option<String> {
        let start_position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        self.input
            .get(start_position..self.position)
            .map(|s| s.iter().collect::<String>())
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }
}

fn is_letter(ch: char) -> bool {
    ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TokenTest {
        expected_type: TokenType,
        expected_literal: String,
    }

    #[test]
    fn test_next_token1() {
        let input = "=+(){},;";

        let mut tests = Vec::new();
        tests.push(TokenTest {
            expected_type: TokenType::Assign,
            expected_literal: "=".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Plus,
            expected_literal: "+".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::LParen,
            expected_literal: "(".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::RParen,
            expected_literal: ")".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::LBrace,
            expected_literal: "{".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::RBrace,
            expected_literal: "}".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Comma,
            expected_literal: ",".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Semicolon,
            expected_literal: ";".to_string(),
        });

        let mut l = Lexer::new(input.to_string());

        for (i, t) in tests.into_iter().enumerate() {
            println!("Test {}", i);
            let tok = l.next_token();
            assert_eq!(tok.token_type, t.expected_type);
            assert_eq!(tok.literal, t.expected_literal);
        }
    }

    #[test]
    fn test_next_token2() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
    "#;

        let mut tests = Vec::new();
        tests.push(TokenTest {
            expected_type: TokenType::Let,
            expected_literal: "let".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "five".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Assign,
            expected_literal: "=".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Int,
            expected_literal: "5".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Semicolon,
            expected_literal: ";".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Let,
            expected_literal: "let".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "ten".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Assign,
            expected_literal: "=".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Int,
            expected_literal: "10".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Semicolon,
            expected_literal: ";".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Let,
            expected_literal: "let".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "add".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Assign,
            expected_literal: "=".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Function,
            expected_literal: "fn".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::LParen,
            expected_literal: "(".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "x".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Comma,
            expected_literal: ",".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "y".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::RParen,
            expected_literal: ")".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::LBrace,
            expected_literal: "{".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "x".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Plus,
            expected_literal: "+".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "y".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Semicolon,
            expected_literal: ";".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::RBrace,
            expected_literal: "}".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Semicolon,
            expected_literal: ";".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Let,
            expected_literal: "let".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "result".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Assign,
            expected_literal: "=".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "add".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::LParen,
            expected_literal: "(".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "five".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Comma,
            expected_literal: ",".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Identifier,
            expected_literal: "ten".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::RParen,
            expected_literal: ")".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Semicolon,
            expected_literal: ";".to_string(),
        });

        let mut l = Lexer::new(input.to_string());

        for (i, t) in tests.into_iter().enumerate() {
            let tok = l.next_token();
            println!("Test {}", i);
            assert_eq!(tok.token_type, t.expected_type);
            assert_eq!(tok.literal, t.expected_literal);
        }
    }

    #[test]
    fn test_next_token3() {
        let input = "<>!-/*5";

        let mut tests = Vec::new();
        tests.push(TokenTest {
            expected_type: TokenType::Lt,
            expected_literal: "<".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Gt,
            expected_literal: ">".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Bang,
            expected_literal: "!".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Minus,
            expected_literal: "-".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Slash,
            expected_literal: "/".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Asterisk,
            expected_literal: "*".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Int,
            expected_literal: "5".to_string(),
        });

        let mut l = Lexer::new(input.to_string());

        for (i, t) in tests.into_iter().enumerate() {
            let tok = l.next_token();
            println!("Test {}", i);
            assert_eq!(tok.token_type, t.expected_type);
            assert_eq!(tok.literal, t.expected_literal);
        }
    }

    #[test]
    fn test_next_token4() {
        let input = r#"
if (5 < 10) {
    return true;
} else {
    return false;
}
            "#;

        let mut tests = Vec::new();
        tests.push(TokenTest {
            expected_type: TokenType::If,
            expected_literal: "if".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::LParen,
            expected_literal: "(".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Int,
            expected_literal: "5".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Lt,
            expected_literal: "<".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Int,
            expected_literal: "10".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::RParen,
            expected_literal: ")".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::LBrace,
            expected_literal: "{".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Return,
            expected_literal: "return".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::True,
            expected_literal: "true".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Semicolon,
            expected_literal: ";".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::RBrace,
            expected_literal: "}".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Else,
            expected_literal: "else".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::LBrace,
            expected_literal: "{".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Return,
            expected_literal: "return".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::False,
            expected_literal: "false".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Semicolon,
            expected_literal: ";".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::RBrace,
            expected_literal: "}".to_string(),
        });

        let mut l = Lexer::new(input.to_string());

        for (i, t) in tests.into_iter().enumerate() {
            let tok = l.next_token();
            println!("Test {}", i);
            assert_eq!(tok.token_type, t.expected_type);
            assert_eq!(tok.literal, t.expected_literal);
        }
    }

    #[test]
    fn test_next_token5() {
        let input = r#"
10 == 10;
10 != 9;
            "#;

        let mut tests = Vec::new();
        tests.push(TokenTest {
            expected_type: TokenType::Int,
            expected_literal: "10".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Eq,
            expected_literal: "==".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Int,
            expected_literal: "10".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Semicolon,
            expected_literal: ";".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Int,
            expected_literal: "10".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::NotEq,
            expected_literal: "!=".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Int,
            expected_literal: "9".to_string(),
        });
        tests.push(TokenTest {
            expected_type: TokenType::Semicolon,
            expected_literal: ";".to_string(),
        });

        let mut l = Lexer::new(input.to_string());

        for (i, t) in tests.into_iter().enumerate() {
            let tok = l.next_token();
            println!("Test {}", i);
            assert_eq!(tok.token_type, t.expected_type);
            assert_eq!(tok.literal, t.expected_literal);
        }
    }
}

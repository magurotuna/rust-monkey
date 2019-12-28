use crate::token::{Token, TokenType};
use std::default::Default;

#[derive(Default, Debug)]
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    fn new(input: String) -> Self {
        let mut l = Lexer {
            input: input.chars().collect(),
            ..Default::default()
        };
        l.read_char();
        l
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

    fn next_token(&mut self) -> Token {
        let token = match self.ch {
            '=' => Token::new(TokenType::Assign, self.ch.to_string()),
            ';' => Token::new(TokenType::Semicolon, self.ch.to_string()),
            '(' => Token::new(TokenType::LParen, self.ch.to_string()),
            ')' => Token::new(TokenType::RParen, self.ch.to_string()),
            ',' => Token::new(TokenType::Comma, self.ch.to_string()),
            '+' => Token::new(TokenType::Plus, self.ch.to_string()),
            '{' => Token::new(TokenType::LBrace, self.ch.to_string()),
            '}' => Token::new(TokenType::RBrace, self.ch.to_string()),
            c if (c as u8) == 0 => Token::new(TokenType::Eof, "".to_string()),
            _ => Token::new(TokenType::Identifier, "test".to_string()),
        };
        self.read_char();
        token
    }
}

#[test]
fn test_next_token() {
    let input = "=+(){},;";

    struct TokenTest {
        expected_type: TokenType,
        expected_literal: String,
    };
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

    for t in tests {
        let tok = l.next_token();
        assert_eq!(tok.token_type, t.expected_type);
        assert_eq!(tok.literal, t.expected_literal);
    }
}

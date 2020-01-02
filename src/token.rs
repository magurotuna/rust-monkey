use std::collections::HashMap;
use std::default::Default;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn", TokenType::Function);
        m.insert("let", TokenType::Let);
        m.insert("if", TokenType::If);
        m.insert("else", TokenType::Else);
        m.insert("return", TokenType::Return);
        m.insert("true", TokenType::True);
        m.insert("false", TokenType::False);
        m
    };
}

pub fn lookup_identifier(identifier: &str) -> TokenType {
    match (*KEYWORDS).get(identifier) {
        Some(&token_type) => token_type,
        None => TokenType::Identifier,
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Token {
            token_type,
            literal,
        }
    }

    pub fn is_identifier_or_keywords(&self) -> bool {
        match self.token_type {
            TokenType::Identifier
            | TokenType::Function
            | TokenType::Let
            | TokenType::Int
            | TokenType::If
            | TokenType::Else
            | TokenType::Return
            | TokenType::True
            | TokenType::False => true,
            _ => false,
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            token_type: TokenType::Illegal,
            literal: "".to_string(),
        }
    }
}

impl From<String> for Token {
    fn from(literal: String) -> Self {
        Token::new(TokenType::Identifier, literal)
    }
}

impl From<&str> for Token {
    fn from(literal: &str) -> Self {
        Token::new(TokenType::Identifier, literal.to_string())
    }
}

impl From<i64> for Token {
    fn from(value: i64) -> Self {
        Token::new(TokenType::Int, value.to_string())
    }
}

impl From<bool> for Token {
    fn from(value: bool) -> Self {
        Token::new(
            if value {
                TokenType::True
            } else {
                TokenType::False
            },
            value.to_string(),
        )
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum TokenType {
    Illegal,
    Eof,
    Identifier,
    Int,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

use lazy_static::lazy_static;
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
    pub fn new<T: Into<String>>(token_type: TokenType, literal: T) -> Self {
        Token {
            token_type,
            literal: literal.into(),
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

// To use trait object, prepare this trait which is object safety.
pub trait IntoToken {
    fn into_token(&self) -> Token;
}

impl IntoToken for i64 {
    fn into_token(&self) -> Token {
        Token::new(TokenType::Int, self.to_string())
    }
}

impl IntoToken for &str {
    fn into_token(&self) -> Token {
        Token::new(TokenType::Identifier, self.to_string())
    }
}

impl IntoToken for String {
    fn into_token(&self) -> Token {
        Token::new(TokenType::Identifier, self.clone())
    }
}

impl IntoToken for bool {
    fn into_token(&self) -> Token {
        Token::new(
            if *self {
                TokenType::True
            } else {
                TokenType::False
            },
            self.to_string(),
        )
    }
}

impl<T: ?Sized + IntoToken> IntoToken for Box<T> {
    fn into_token(&self) -> Token {
        (**self).into_token()
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

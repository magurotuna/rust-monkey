use std::collections::HashMap;

lazy_static! {
    static ref keywords: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn", TokenType::Function);
        m.insert("let", TokenType::Let);
        m
    };
}

pub fn lookup_identifier(identifier: &str) -> TokenType {
    match (*keywords).get(identifier) {
        Some(&token_type) => token_type,
        None => TokenType::Identifier,
    }
}

#[derive(Debug)]
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

    pub fn is_not_symbol(&self) -> bool {
        match self.token_type {
            TokenType::Identifier | TokenType::Function | TokenType::Let | TokenType::Int => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenType {
    Illegal,
    Eof,
    Identifier,
    Int,
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
}

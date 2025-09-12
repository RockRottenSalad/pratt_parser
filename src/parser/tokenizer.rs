#![allow(dead_code)] 

use crate::token::{*};
use std::fmt;

pub struct Tokenizer {
    tokens: Vec<Token>,
    index: usize
}

impl Tokenizer {
    pub fn new(text: &str) -> Result<Self, (TokenizerError, usize)> {
        let mut tokens: Vec<Token> = Vec::new();
        tokens.reserve(text.len());

        for (i, token) in text.chars().map(|x| char_to_token(x)).enumerate() {
            match token {
                Ok(t) => tokens.push(t),
                Err(e) => return Err((e, i))
            }
        }

        Ok(Tokenizer {tokens, index: 0})
    }

    pub fn next(&mut self) -> Token {
        if self.index == self.tokens.len() {
            return Token::EOF;
        }

        let ret = &self.tokens[self.index];
        self.index += 1;

        ret.clone()
    }

    pub fn peek(&self) -> Token {
        if self.index == self.tokens.len() {
            return Token::EOF;
        }

        self.tokens[self.index].clone()
    }

    pub fn tokens_remaining(&self) -> usize {
        self.tokens.len() - self.index
    }

    pub fn reset(&mut self) {
        self.index = 0;
    }
}

impl fmt::Display for Tokenizer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Tokenizer [ tokens: {:?}, index: {} ]", self.tokens, self.index)
    }
}



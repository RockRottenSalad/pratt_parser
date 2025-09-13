#![allow(dead_code)] 

use std::fmt;

use std::iter::Peekable;
use std::{str::CharIndices, vec::Vec};

#[derive(Debug)]
pub enum TokenizerError {
    IllegalToken(char)
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenizerError::IllegalToken(ch) => write!(f, "IllegalToken({ch})")
        }
    }
}

#[derive(Debug,Clone)]
pub enum Token {
    Literal(i32),
    Plus,
    Minus,
    Star,
    Slash,
    ParenR,
    ParenL,
    Space,
    EOF,
}

impl Token {
    pub fn precedence(self) -> u8 {
        match self {
            Token::EOF | Token::Space => 0,

            Token::Literal(_) => 0,

            Token::Plus => 1,
            Token::Minus => 1,

            Token::Star => 2,
            Token::Slash => 2,

            Token::ParenR => 1,
            Token::ParenL => 3,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Literal(x) => write!(f, "Literal({x})"),
            Token::Plus => write!(f, "Plus(+)"),
            Token::Minus => write!(f, "Minus(-)"),
            Token::Star => write!(f, "Star(*)"),
            Token::Slash => write!(f, "Slash(/)"),
            Token::ParenR => write!(f, "ParenR())"),
            Token::ParenL => write!(f, "Slash(()"),
            Token::EOF => write!(f, "EOF"),
            Token::Space => write!(f, " "),
        }
    }
}

pub fn char_to_token(ch: char) -> Result<Token, TokenizerError> {
    match ch {
        '+' => Ok(Token::Plus),
        '-' => Ok(Token::Minus),
        '*' => Ok(Token::Star),
        '/' => Ok(Token::Slash),
        '(' => Ok(Token::ParenL),
        ')' => Ok(Token::ParenR),
        ' ' | '\t' | '\n' => Ok(Token::Space),
        _ => Err(TokenizerError::IllegalToken(ch)) 
    }
}

fn parse_literal(chs: &mut Peekable<CharIndices>) -> i32 {
    let radix = 10;
    let mut sum = 0;

    while let Some(num) = chs
        .next_if(|(_, ch)| ch.is_digit(radix) )
        .map(|(_, ch)| ch.to_digit(radix).unwrap() as i32 ) 
    {
        sum = sum*10 + num;
    }
    sum
}

pub fn tokenize(text: &str) -> Result<Vec<Token>, (TokenizerError, usize)> {
    let radix = 10;
    let mut tokens: Vec<Token> = Vec::new();
    tokens.reserve(text.len());

    let mut chs = text.char_indices().peekable();

    while let Some((i, ch)) = chs.peek() {
        if ch.is_digit(radix) {
            tokens.push( Token::Literal(parse_literal(&mut chs) ));
        } else {
            match char_to_token(*ch) {
                Ok(t) => match t {
                    Token::Space => { chs.next(); },
                    _ => { tokens.push(t); chs.next(); }
                },
                Err(e) => return Err((e, *i))
            }
        }

    }

    Ok(tokens)
}



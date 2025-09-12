#![allow(dead_code)] 

use std::fmt;

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
    EOF
}


impl Token {
    pub fn precedence(self) -> u8 {
        match self {
            Token::EOF => 0,
            Token::Literal(_) => 0,
            Token::Plus => 1,
            Token::Minus => 2
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Literal(x) => write!(f, "Literal({x})"),
            Token::Plus => write!(f, "Plus(+)"),
            Token::Minus => write!(f, "Plus(-)"),
            Token::EOF => write!(f, "EOF")
        }
    }
}

fn digit_char_to_i32(ch: char) -> i32 {
    match ch {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
        _ => panic!("Invalid digit {ch}")
    }
}

pub fn char_to_token(ch: char) -> Result<Token, TokenizerError> {
    match ch {
        '+' => Ok(Token::Plus),
        '-' => Ok(Token::Minus),
        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => Ok(Token::Literal(digit_char_to_i32(ch))),
        _ => Err(TokenizerError::IllegalToken(ch))
    }
}



#![allow(dead_code)] 

use std::fmt;

use std::iter::Peekable;
use std::{str::CharIndices, vec::Vec};

use crate::utils::types::Either;

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

#[derive(Debug,Clone,PartialEq)]
pub enum Token {
    LiteralInteger(i32),
    LiteralReal(f32),
    LiteralBoolean(bool),

    Period,
    Plus,
    Minus,
    Star,
    Slash,
    ParenR,
    ParenL,

    GreaterThan,
    LessThan,

    Equal,
    Bang,

    Space,
    EOF,
}

impl Token {
    pub fn precedence(self) -> u8 {
        match self {
            Token::EOF | Token::Space => 0,

            Token::LiteralInteger(_) => 0,
            Token::LiteralReal(_) => 0,

            Token::GreaterThan => 1,
            Token::LessThan => 1,
            Token::Equal => 1,
            Token::Bang => 1,

            Token::Plus => 2,
            Token::Minus => 2,

            Token::Star => 3,
            Token::Slash => 4,

            // Proper precedence for rest either don't matter or are baked directly into the parser
            _ => 0
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::LiteralInteger(x) => write!(f, "LiteralInteger({x})"),
            Token::LiteralReal(x) => write!(f, "LiteralReal({x})"),
            Token::LiteralBoolean(x) => write!(f, "LiteralReal({x})"),
            Token::Plus => write!(f, "Plus(+)"),
            Token::Minus => write!(f, "Minus(-)"),
            Token::Star => write!(f, "Star(*)"),
            Token::Slash => write!(f, "Slash(/)"),
            Token::ParenR => write!(f, "ParenR())"),
            Token::ParenL => write!(f, "Slash(()"),
            Token::Period => write!(f, "Period(.)"),
            Token::GreaterThan => write!(f, "GreaterThan(>)"),
            Token::LessThan => write!(f, "LessThan(<)"),
            Token::Equal => write!(f, "Equal(<)"),
            Token::Bang => write!(f, "Bang(!)"),
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
        '.' => Ok(Token::Period),
        '>' => Ok(Token::GreaterThan),
        '<' => Ok(Token::LessThan),
        '!' => Ok(Token::Bang),
        '=' => Ok(Token::Equal),
        'T' => Ok(Token::LiteralBoolean(true)),
        'F' => Ok(Token::LiteralBoolean(false)),
        ' ' | '\t' | '\n' => Ok(Token::Space),
        _ => Err(TokenizerError::IllegalToken(ch)) 
    }
}

fn parse_literal_new(chs: &mut Peekable<CharIndices>) -> Either<i32, f32> {
    let before_dot = parse_literal(chs);

    if let Some((_, ch)) = chs.peek() && *ch == '.' {
        let _ = chs.next();

        let before_dot = before_dot as f32;
        let after_dot = parse_literal(chs);

        if after_dot != 0 {
            // TODO: This is a hack, fix it.
            Either::Right(before_dot + (after_dot as f32) / 10.0_f32.powi((after_dot.ilog10()+1) as i32))
        }else {
            Either::Right(before_dot)
        }
    }else {
        Either::Left(before_dot)
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
            match parse_literal_new(&mut chs) {
                Either::Left(x) => tokens.push( Token::LiteralInteger(x)),
                Either::Right(x) => tokens.push( Token::LiteralReal(x)),
            }
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



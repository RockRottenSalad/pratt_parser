#![allow(dead_code)]

use std::fmt;
use std::rc::Rc;

use std::iter::Peekable;
use std::{str::CharIndices, vec::Vec};

use crate::utils::types::Either;

#[derive(Debug)]
pub enum TokenizerError {
    IllegalToken(char),
    UndefinedIdentifier(Box<str>),
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenizerError::IllegalToken(ch) => write!(f, "IllegalToken({ch})"),
            TokenizerError::UndefinedIdentifier(str) => write!(f, "UndefinedIdentifier({str})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LiteralInteger(i32),
    LiteralReal(f32),
    LiteralBoolean(bool),

    Identifier(Rc<str>),

    Period,
    Plus,
    Minus,
    Star,
    Slash,

    ParenR,
    ParenL,
    CurlyR,
    CurlyL,

    If,
    Else,
    Let,
    Print,

    GreaterThan,
    LessThan,

    Equal,
    Bang,

    And,
    Or,

    Space,
    EOF,

    As,
    Int, 
    Real,
    Bool,

    Question,
    Colon,
}

fn token_from_identifier(identifier: &str) -> Option<Token> {
    match identifier {
        "true"  => Some(Token::LiteralBoolean(true)),
        "false" => Some(Token::LiteralBoolean(false)),
        "if"    => Some(Token::If),
        "else"  => Some(Token::Else),
        "let"   => Some(Token::Let),
        "print" => Some(Token::Print),
        "as"    => Some(Token::As),
        "int"   => Some(Token::Int),
        "real"  => Some(Token::Real),
        "bool"  => Some(Token::Bool),
        "and"   => Some(Token::And),
        "or"    => Some(Token::Or),
        _       => None
    }
}

impl Token {
    pub fn precedence(self) -> u8 {
        match self {
            Token::EOF | Token::Space => 0,

            Token::LiteralInteger(_) => 0,
            Token::LiteralReal(_) => 0,
            Token::LiteralBoolean(_) => 0,
            Token::Identifier(_) => 0,

            Token::GreaterThan => 2,
            Token::LessThan => 2,
            Token::Equal => 2,
            Token::Bang => 2,

            Token::Plus => 3,
            Token::Minus => 3,

            Token::Star => 4,
            Token::Slash => 4,

            Token::And => 5,
            Token::Or => 5,

            Token::As => 6,
            // TODO Figure out if ternary operator precedence be handled by parser instead?
            // Might be cleaner, might not
            Token::Question => 1,
            Token::Colon => 0,

            // Proper precedence for rest either don't matter or are baked directly into the parser
            _ => 0,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::LiteralInteger(x) => write!(f, "LiteralInteger({x})"),
            Token::LiteralReal(x) => write!(f, "LiteralReal({x})"),
            Token::LiteralBoolean(x) => write!(f, "LiteralReal({x})"),
            Token::Plus => write!(f, "Plus(+)"),
            Token::Minus => write!(f, "Minus(-)"),
            Token::Star => write!(f, "Star(*)"),
            Token::Slash => write!(f, "Slash(/)"),
            Token::ParenR => write!(f, "ParenR())"),
            Token::ParenL => write!(f, "Slash(()"),
            Token::CurlyR => write!(f, "CurlyR(}})"),
            Token::CurlyL => write!(f, "CurlyL({{)"),
            Token::Period => write!(f, "Period(.)"),
            Token::GreaterThan => write!(f, "GreaterThan(>)"),
            Token::LessThan => write!(f, "LessThan(<)"),
            Token::Equal => write!(f, "Equal(=)"),
            Token::Bang => write!(f, "Bang(!)"),
            Token::Question => write!(f, "Question(?)"),
            Token::Colon => write!(f, "Colon(:)"),
            Token::EOF => write!(f, "EOF"),
            Token::Space => write!(f, "Space(  )"),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::Identifier(str) => write!(f, "Identifier({str})"),
            Token::Let => write!(f, "Let"),
            Token::Print => write!(f, "Print"),
            Token::As => write!(f, "As"),
            Token::And => write!(f, "And"),
            Token::Or => write!(f, "Or"),
            Token::Int => write!(f, "Type(Int)"),
            Token::Real => write!(f, "Type(Real)"),
            Token::Bool => write!(f, "Type(Bool)"),
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
        '{' => Ok(Token::CurlyL),
        '}' => Ok(Token::CurlyR),
        '.' => Ok(Token::Period),
        '>' => Ok(Token::GreaterThan),
        '<' => Ok(Token::LessThan),
        '!' => Ok(Token::Bang),
        '=' => Ok(Token::Equal),
        '?' => Ok(Token::Question),
        ':' => Ok(Token::Colon),
        ' ' | '\t' | '\n' => Ok(Token::Space),
        _ => Err(TokenizerError::IllegalToken(ch)),
    }
}

fn parse_numeric_literal_new(chs: &mut Peekable<CharIndices>) -> Either<i32, f32> {
    let before_dot = parse_integer_literal(chs);

    if let Some((_, ch)) = chs.peek()
        && *ch == '.'
    {
        let _ = chs.next();

        let before_dot = before_dot as f32;
        let after_dot = parse_integer_literal(chs);

        if after_dot != 0 {
            // TODO: This is a hack, fix it.
            Either::Right(
                before_dot + (after_dot as f32) / 10.0_f32.powi((after_dot.ilog10() + 1) as i32),
            )
        } else {
            Either::Right(before_dot)
        }
    } else {
        Either::Left(before_dot)
    }
}

fn parse_integer_literal(chs: &mut Peekable<CharIndices>) -> i32 {
    let radix = 10;
    let mut sum = 0;

    while let Some(num) = chs
        .next_if(|(_, ch)| ch.is_digit(radix))
        .map(|(_, ch)| ch.to_digit(radix).unwrap() as i32)
    {
        sum = sum * 10 + num;
    }
    sum
}

// NOTE: Explodes if called whilst at EOF
fn parse_identifier_or_keyword<'a>(chs: &mut Peekable<CharIndices>, text: &str) -> Result<Token, TokenizerError> {

    let start = match chs.peek() {
        Some(&(i, _)) => i,
        None => panic!("'parse_identifier_or_keyword()' was called whilst at EOF - this should never happen")
    };

    // Why must .take_while() consume the final element which does not meet the predicate?
    // chs is peekable
    // that is so annoyingly inconvinent.
    // If I wanted that behaviour I could just do a .skip(1) after my .take_while()
    // There is no .rewind(1)
    let end = {
        let mut end = 0;
        while let Some(_) = chs.next_if(|(_, ch)| ch.is_alphabetic()) {
            end += 1;
        }
        start + end
    };

    let identifier = &text[start..end];

    // TODO as more keywords are added, a better way of handing this is needed.
    // Maybe Radix Trie if it gets really bad?
    match token_from_identifier(identifier) {
        Some(t) => Ok(t),
        None => Ok(Token::Identifier(identifier.into())),
    }
}

pub fn tokenize(text: &str) -> Result<Vec<Token>, (TokenizerError, usize)> {
    let radix = 10;
    let mut tokens: Vec<Token> = Vec::with_capacity(text.len());

    let mut chs = text.char_indices().peekable();

    while let Some(&(i, ch)) = chs.peek() {
        if ch.is_digit(radix) {
            match parse_numeric_literal_new(&mut chs) {
                Either::Left(integer) => tokens.push(Token::LiteralInteger(integer)),
                Either::Right(real) => tokens.push(Token::LiteralReal(real)),
            }
        } else if ch.is_alphabetic() {
            match parse_identifier_or_keyword(&mut chs, text) {
                Ok(tok) => tokens.push(tok),
                Err(e) => return Err((e, i)),
            }
        } else {
            chs.next();
            match char_to_token(ch) {
                Err(e) => return Err((e, i)),
                Ok(t) => match t {
                    Token::Space => { /* do nothing */ },
                    _            => tokens.push(t),
                },
            }
        }
    }

    Ok(tokens)
}

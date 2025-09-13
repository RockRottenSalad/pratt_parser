#![allow(dead_code)] 

// TODO
// Should tolerate invalid tokens i.e. Space tokens if they make their way through the tokenizer
// More fault tolerance + should return Result type

use crate::{parser::{ast::{Expression, Operator}}, token::Token};

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    index: usize
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser { tokens, index: 0 }
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

    pub fn index(&self) -> usize {
        // Only used for error reporting. We want the index of the token we just consumed, not the
        // index we're about to consume.
        //
        // The zero check is to prevent this from blowing up if it's called before any tokens are
        // conumsed for some reason
        if self.index == 0 { self.index } else { self.index - 1 }
    }

}

#[derive(Debug)]
pub enum ParserError {
    UnterminatedGrouping(usize),
    ExpectedOperator(usize),
    ExpectedLiteral(usize),
    SyntaxError(usize),
}


fn head_handler(token: Token, p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    match token {
        Token::Literal(x) => Ok(Box::new( Expression::Literal(x)  )),
        Token::Minus => Ok(Box::new( Expression::Unary(Operator::Subtraction, parse_expr(p, token.precedence())? ) )),
        Token::Plus => Ok(Box::new( Expression::Unary(Operator::Addition, parse_expr(p, token.precedence())? ) )),
        Token::ParenL => {
            let expr = parse_expr(p, 0)?;
            match p.next() {
                Token::ParenR => Ok(Box::new( Expression::Grouping(expr) )),
                _ => Err(ParserError::UnterminatedGrouping(p.index()))
        } },
        _ => Err(ParserError::ExpectedLiteral(p.index()))
    }
}

fn tail_handler(token: Token, expr: Box<Expression>, p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    match token {
        Token::Literal(_) => Err(ParserError::ExpectedOperator(p.index())),
        Token::Plus => Ok(Box::new( Expression::Binary(Operator::Addition, expr, parse_expr(p, token.precedence())?) )),
        Token::Minus => Ok(Box::new( Expression::Binary(Operator::Subtraction, expr, parse_expr(p, token.precedence())?) )),
        Token::Star => Ok(Box::new( Expression::Binary(Operator::Multiplication, expr, parse_expr(p, token.precedence())?) )),
        Token::Slash => Ok(Box::new( Expression::Binary(Operator::Division, expr, parse_expr(p, token.precedence())?) )),
        _ => Err(ParserError::SyntaxError(p.index()))
    }
}

fn parse_expr(p: &mut Parser, expr_precedence: u8) -> Result<Box<Expression>, ParserError> {
    let mut current = p.next();
    let mut left_expr = head_handler(current, p)?;

    while p.peek().precedence() > expr_precedence {
        current = p.next();
        left_expr = tail_handler(current, left_expr, p)?;

    }

    return Ok(left_expr);
}

pub fn parse(t: &Vec<Token>) -> Result<Box<Expression>, ParserError> {
    return parse_expr(&mut Parser::new(t), 0);
}


#![allow(dead_code)] 

// TODO
// Should tolerate invalid tokens i.e. Space tokens if they make their way through the tokenizer
// More fault tolerance + should return Result type

use crate::{parser::{ast::{Expression, LiteralKind}}, token::Token};

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    index: usize
}

struct Example {
    iter: Box<dyn Iterator<Item = u32>>,
}


impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser { tokens, index: 0 }
    }

    pub fn consume(&mut self) {
        if self.tokens_remaining() > 0 {
            self.index += 1;
        }
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
        self.index
    }

}

#[derive(Debug,PartialEq)]
pub enum ParserError {
    UnterminatedGrouping(usize),
    ExpectedOperator(usize),
    ExpectedLiteral(usize),
    SyntaxError(usize),
}


fn head_handler(token: Token, p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    match token {
        Token::LiteralInteger(x) => Ok(Box::new( Expression::Literal(LiteralKind::Integer(x))  )),
        Token::LiteralReal(x) => Ok(Box::new( Expression::Literal(LiteralKind::Real(x))  )),
        Token::LiteralBoolean(x) => Ok(Box::new( Expression::Literal(LiteralKind::Boolean(x))  )),
        Token::Minus => Ok(Box::new( Expression::UnaryNegation(parse_expr(p, token.precedence())? ) )),
        Token::Plus => Ok(Box::new( Expression::UnaryAddition(parse_expr(p, token.precedence())? ) )),
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
        Token::LiteralInteger(_) => Err(ParserError::ExpectedOperator(p.index())),
        Token::LiteralReal(_) => Err(ParserError::ExpectedOperator(p.index())),
        Token::Plus => Ok(Box::new( Expression::BinaryAddition(expr, parse_expr(p, token.precedence())?) )),
        Token::Minus => Ok(Box::new( Expression::BinarySubtraction(expr, parse_expr(p, token.precedence())?) )),
        Token::Star => Ok(Box::new( Expression::BinaryMultiplication(expr, parse_expr(p, token.precedence())?) )),
        Token::Slash => Ok(Box::new( Expression::BinaryDivision(expr, parse_expr(p, token.precedence())?) )),

        Token::GreaterThan => 
            if p.peek() == Token::Equal  {
                p.consume();
                Ok(Box::new( Expression::GreaterEqualThan(expr, parse_expr(p, token.precedence())?) ))
            }else {
                Ok(Box::new( Expression::GreaterThan(expr, parse_expr(p, token.precedence())?) ))
            },
        Token::LessThan => 
            if p.peek() == Token::Equal  {
                p.consume();
                Ok(Box::new( Expression::LessEqualThan(expr, parse_expr(p, token.precedence())?) ))
            }else {
                Ok(Box::new( Expression::LessThan(expr, parse_expr(p, token.precedence())?) ))
            },

        Token::Equal => 
            if p.peek() == Token::Equal  {
                p.consume();
                Ok(Box::new( Expression::EqualTo(expr, parse_expr(p, token.precedence())?) ))
            }else {
                Err(ParserError::SyntaxError(p.index()))
            },

        Token::Bang => 
            if p.peek() == Token::Equal  {
                p.consume();
                Ok(Box::new( Expression::NotEqualTo(expr, parse_expr(p, token.precedence())?) ))
            }else {
                Err(ParserError::SyntaxError(p.index()))
            },



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

    Ok(left_expr)
}

pub fn parse(t: &Vec<Token>) -> Result<Box<Expression>, ParserError> {
    let mut parser = Parser::new(t);
    let res = parse_expr(&mut parser, 0);

    if res.is_ok() && parser.peek() != Token::EOF {
        Err(ParserError::ExpectedOperator(parser.index()))
    }else {
        res
    }

}


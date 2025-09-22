#![allow(dead_code)]

// TODO
// Should tolerate invalid tokens i.e. Space tokens if they make their way through the tokenizer
// More fault tolerance + should return Result type

use crate::{
    parser::ast::{Expression, LiteralKind},
    token::Token,
};
use std::fmt;

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    index: usize,
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

#[derive(Debug, PartialEq)]
pub enum ParserError {
    UnterminatedGrouping(usize),
    ExpectedOperator(usize),
    ExpectedLiteral(usize),
    SyntaxError(usize),
    ExpectedToken(usize, Token),
}

// TODO: Since there is a tokens array, that can be used alongside the index to give a better error
// message. The index of a token doesn't map well to a string. Especially when you factor in the
// fact that '>=' is counted as two tokens
impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::UnterminatedGrouping(x) => write!(f, "Unterminated grouping at {x}"),
            ParserError::ExpectedOperator(x) => write!(f, "Expected operator at {x}"),
            ParserError::ExpectedLiteral(x) => write!(f, "Expected literal at {x}"),
            ParserError::SyntaxError(x) => write!(f, "Syntax error at {x}"),
            ParserError::ExpectedToken(x, t) => write!(f, "Expected token {t} at {x}"),
        }
    }
}

fn consume_until_token_or_eof(token: &Token, p: &mut Parser) -> () {
    if p.peek() != *token {
        while p.peek() != *token && p.peek() != Token::EOF {
            p.consume();
        }
    }
}

fn expect(token: Token, p: &mut Parser) -> Result<(), ParserError> {
    if p.peek() != token {
        let index = p.index();
        consume_until_token_or_eof(&token, p);
        Err(ParserError::ExpectedToken(index, token))
    } else {
        p.consume();
        Ok(())
    }
}

fn grouping_expr_handler(p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    let expr = parse_expr(p, 0)?;

    match expect(Token::ParenR, p) {
        Err(e) => match e {
            ParserError::ExpectedToken(i, _) => return Err(ParserError::UnterminatedGrouping(i)),
            _ => panic!("Expect should currently only retturn expected token errors"),
        },
        Ok(()) => {}
    };

    Ok(Box::new(Expression::Grouping(expr)))
}

fn if_statement_handler(p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    let condition = parse_expr(p, 0)?;

    expect(Token::CurlyL, p)?;

    let statement = parse_expr(p, 0)?;

    expect(Token::CurlyR, p)?;

    expect(Token::Else, p)?;

    Ok(Box::new(Expression::Ternary(
        condition,
        statement,
        parse_expr(p, 0)?,
    )))
}

fn block_statement_handler(p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    let expr = parse_expr(p, 0)?;
    expect(Token::CurlyR, p)?;
    Ok(expr)
}

fn head_handler(token: Token, p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    match token {
        // IF statement
        Token::If => if_statement_handler(p),
        Token::CurlyL => block_statement_handler(p),

        Token::LiteralInteger(x) => Ok(Box::new(Expression::Literal(LiteralKind::Integer(x)))),
        Token::LiteralReal(x) => Ok(Box::new(Expression::Literal(LiteralKind::Real(x)))),
        Token::LiteralBoolean(x) => Ok(Box::new(Expression::Literal(LiteralKind::Boolean(x)))),
        Token::Minus => Ok(Box::new(Expression::UnaryNegation(parse_expr(
            p,
            token.precedence(),
        )?))),
        Token::Plus => Ok(Box::new(Expression::UnaryAddition(parse_expr(
            p,
            token.precedence(),
        )?))),
        Token::ParenL => grouping_expr_handler(p),
        _ => Err(ParserError::ExpectedLiteral(p.index())),
    }
}

fn tail_handler(token: Token, expr: Box<Expression>, p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    match token {
        Token::LiteralInteger(_) => Err(ParserError::ExpectedOperator(p.index())),
        Token::LiteralReal(_) => Err(ParserError::ExpectedOperator(p.index())),
        Token::Plus => Ok(Box::new(Expression::BinaryAddition(
            expr,
            parse_expr(p, token.precedence())?,
        ))),
        Token::Minus => Ok(Box::new(Expression::BinarySubtraction(
            expr,
            parse_expr(p, token.precedence())?,
        ))),
        Token::Star => Ok(Box::new(Expression::BinaryMultiplication(
            expr,
            parse_expr(p, token.precedence())?,
        ))),
        Token::Slash => Ok(Box::new(Expression::BinaryDivision(
            expr,
            parse_expr(p, token.precedence())?,
        ))),

        Token::Question => {
            let left = parse_expr(p, 0)?;

            if p.peek() != Token::Colon {
                return Err(ParserError::SyntaxError(p.index()));
            } else {
                p.consume();
            }

            // TODO implement expected an expression error and use that here
            if p.peek() == Token::EOF {
                return Err(ParserError::ExpectedLiteral(p.index()));
            }

            let right = parse_expr(p, 0)?;

            Ok(Box::new(Expression::Ternary(expr, left, right)))
        }
        Token::GreaterThan => {
            if p.peek() == Token::Equal {
                p.consume();
                Ok(Box::new(Expression::GreaterEqualThan(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            } else {
                Ok(Box::new(Expression::GreaterThan(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            }
        }
        Token::LessThan => {
            if p.peek() == Token::Equal {
                p.consume();
                Ok(Box::new(Expression::LessEqualThan(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            } else {
                Ok(Box::new(Expression::LessThan(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            }
        }
        Token::Equal => {
            if p.peek() == Token::Equal {
                p.consume();
                Ok(Box::new(Expression::EqualTo(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            } else {
                Err(ParserError::SyntaxError(p.index()))
            }
        }
        Token::Bang => {
            if p.peek() == Token::Equal {
                p.consume();
                Ok(Box::new(Expression::NotEqualTo(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            } else {
                Err(ParserError::SyntaxError(p.index()))
            }
        }
        _ => {
            println!("TOKEN: {token}");
            Err(ParserError::SyntaxError(p.index()))
        }
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

pub fn parse(p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    parse_expr(p, 0)
}


#![allow(dead_code)] 

// TODO
// Should tolerate invalid tokens i.e. Space tokens if they make their way through the tokenizer

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

}


fn head_handler(token: Token, p: &mut Parser) -> Box<Expression> {
    match token {
        Token::Literal(x) => Box::new( Expression::Literal(x)  ),
        Token::Minus => Box::new( Expression::Unary(Operator::Subtraction, parse_expr(p, token.precedence())) ),
        Token::Plus => Box::new( Expression::Unary(Operator::Addition, parse_expr(p, token.precedence())) ),
        Token::ParenL => parse_expr(p, 0),
        _ => { panic!("{token} cannot be placed at the head of an expression")  }
    }
}

fn tail_handler(token: Token, expr: Box<Expression>, p: &mut Parser) -> Box<Expression> {
    match token {
        Token::Literal(_) => {panic!("Expected operator after literal")},
        Token::Plus => { Box::new( Expression::Binary(Operator::Addition, expr, parse_expr(p, token.precedence())) )  },
        Token::Minus => { Box::new( Expression::Binary(Operator::Subtraction, expr, parse_expr(p, token.precedence())) )  },
        Token::Star => { Box::new( Expression::Binary(Operator::Multiplication, expr, parse_expr(p, token.precedence())) )  },
        Token::Slash => { Box::new( Expression::Binary(Operator::Division, expr, parse_expr(p, token.precedence())) )  },
        Token::ParenR => Box::new( Expression::Grouping(expr)  ),
        _ => {panic!("{token} cannot be placed at the tail of an expression")}
    }
}

fn parse_expr(p: &mut Parser, expr_precedence: u8) -> Box<Expression> {
    let mut current = p.next();
    let mut left_expr = head_handler(current, p);

    while p.peek().precedence() > expr_precedence {
        current = p.next();
        left_expr = tail_handler(current, left_expr, p);

    }

    return left_expr;
}

pub fn parse(t: &Vec<Token>) -> Box<Expression> {
    return parse_expr(&mut Parser::new(t), 0);
}


#![allow(dead_code)] 

use crate::{parser::{ast::{Expression, Operator}, tokenizer::Tokenizer}, token::Token};


fn head_handler(token: Token, _t: &mut Tokenizer) -> Box<Expression> {
    match token {
        Token::Literal(x) => Box::new( Expression::Literal(x)  ),
        _ => { panic!("WIP")  }
    }
}

fn tail_handler(token: Token, expr: Box<Expression>, t: &mut Tokenizer) -> Box<Expression> {
    match token {
        Token::Literal(_) => {panic!("WIP")},
        Token::Plus => { Box::new( Expression::Binary(Operator::Plus, expr, parse_expr(t, token.precedence())) )  },
        Token::Minus => { Box::new( Expression::Binary(Operator::Minus, expr, parse_expr(t, token.precedence())) )  },
        Token::EOF => {panic!("WIP")}
    }
}

fn parse_expr(t: &mut Tokenizer, expr_precedence: u8) -> Box<Expression> {
    let mut current = t.next();
    let mut left_expr = head_handler(current, t);

    while t.peek().precedence() > expr_precedence {
        current = t.next();
        left_expr = tail_handler(current, left_expr, t);

    }

    return left_expr;
}


pub fn parse(t: &mut Tokenizer) -> Box<Expression> {
    return parse_expr(t, 0);
}


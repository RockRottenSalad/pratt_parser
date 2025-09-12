#![allow(dead_code)] 

use std::fmt;

pub enum Operator {
    Plus,
    Minus,
}

impl Operator {
    pub fn to_char(&self) -> char {
        match self {
            Operator::Plus => '+',
            Operator::Minus => '-',
        }
    }

}

//pub enum LiteralValues {
//    Number(i32)
//}

pub enum Expression {
    Binary(Operator, Box<Expression>, Box<Expression>),
    Literal(i32)
}

impl Expression {

    pub fn evaluate(&self) -> i32 {
        match self {
            Expression::Literal(w) => *w,
            Expression::Binary(op, left, right) => match op {
                Operator::Plus => left.evaluate() + right.evaluate(),
                Operator::Minus => left.evaluate() - right.evaluate()
            }
        }

    } 
}



impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Literal(x) => write!(f, "{x}"),
            Expression::Binary(op, l, r) => write!(f, "({} {l} {r})", op.to_char())
        }
    }
}


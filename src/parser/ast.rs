#![allow(dead_code)] 

use std::fmt;

pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

impl Operator {
    pub fn to_char(&self) -> char {
        match self {
            Operator::Addition => '+',
            Operator::Subtraction => '-',
            Operator::Multiplication => '*',
            Operator::Division => '/',
        }
    }

}

//pub enum LiteralValues {
//    Number(i32)
//}

pub enum Expression {
    Binary(Operator, Box<Expression>, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(i32)
}

impl Expression {

    pub fn evaluate(&self) -> i32 {
        match self {
            Expression::Literal(w) => *w,
            Expression::Grouping(expr) => expr.evaluate(),
            Expression::Binary(op, left, right) => match op {
                Operator::Addition => left.evaluate() + right.evaluate(),
                Operator::Subtraction => left.evaluate() - right.evaluate(),
                Operator::Multiplication => left.evaluate() * right.evaluate(),
                Operator::Division => left.evaluate() / right.evaluate(),
            }
            Expression::Unary(op, expr) => match op {
                Operator::Addition => expr.evaluate(),
                Operator::Subtraction => -expr.evaluate(),
                Operator::Multiplication => panic!("Multiplication is not a unary operator"),
                Operator::Division => panic!("Division is not a unary operator"),
            }
        }

    } 
}



impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Literal(x) => write!(f, "{x}"),
            Expression::Grouping(e) => write!(f, "({e})"),
            Expression::Binary(op, l, r) => write!(f, "({} {l} {r})", op.to_char()),
            Expression::Unary(op, e) => write!(f, "({} {e})", op.to_char())
        }
    }
}


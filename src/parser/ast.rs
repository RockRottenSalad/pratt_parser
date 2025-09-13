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

#[derive(Debug)]
pub enum AstError {
    DivisionByZero,
    IllegalUnaryOperator
}

pub enum Expression {
    Binary(Operator, Box<Expression>, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(i32)
}

impl Expression {

    pub fn evaluate(&self) -> Result<i32, AstError> {
        match self {
            Expression::Literal(w) => Ok(*w),
            Expression::Grouping(expr) => expr.evaluate(),
            Expression::Binary(op, left, right) => match op {
                Operator::Addition => Ok(left.evaluate()? + right.evaluate()?),
                Operator::Subtraction => Ok(left.evaluate()? - right.evaluate()?),
                Operator::Multiplication => Ok(left.evaluate()? * right.evaluate()?),
                Operator::Division => {
                    let left = left.evaluate()?;
                    let right = right.evaluate()?;
                    if right == 0 {
                        Err(AstError::DivisionByZero)
                    }else {
                        Ok(left / right)
                    }
                },
            }
            Expression::Unary(op, expr) => match op {
                Operator::Addition => expr.evaluate(),
                Operator::Subtraction => Ok(-expr.evaluate()?),
                Operator::Multiplication => Err(AstError::IllegalUnaryOperator),
                Operator::Division => Err(AstError::IllegalUnaryOperator),
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


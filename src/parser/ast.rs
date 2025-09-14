#![allow(dead_code)] 

use std::fmt;
use std::ops::{*};

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

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Integer(i32),
    Real(f32)
}

impl LiteralKind {
    pub fn is_zero(&self) -> bool {
        match self {
            LiteralKind::Integer(x) => *x == 0,
            LiteralKind::Real(x) => *x == 0.0,
        }
    }

    pub fn typecast(&self, other: &Self) -> Self {
        match self {
            LiteralKind::Integer(_) => {
                match other {
                    LiteralKind::Integer(_) => other.clone(),
                    LiteralKind::Real(x) => LiteralKind::Integer(*x as i32),
                }
            },
            LiteralKind::Real(_) => {
                match other {
                    LiteralKind::Integer(x) => LiteralKind::Real(*x as f32),
                    LiteralKind::Real(_) => other.clone()
                }
            }
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            LiteralKind::Integer(_) => 0,
            LiteralKind::Real(_) => 1,
        }
    }

}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralKind::Real(x) => write!(f, "{x}"),
            LiteralKind::Integer(x) => write!(f, "{x}"),
        }
    }
}



// These trait implementations hurt to look at
// Maybe it can be fixed with a typecast function which determines which
impl Add for LiteralKind {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match self {
            LiteralKind::Integer(x) => match other {
                LiteralKind::Integer(y) => LiteralKind::Integer(x + y),
                LiteralKind::Real(y) => LiteralKind::Real((x as f32) + y),
            },
            LiteralKind::Real(x) => match other {
                LiteralKind::Integer(y) => LiteralKind::Real(x + (y as f32)),
                LiteralKind::Real(y) => LiteralKind::Real(x + y),
            }
        }
    }
}

impl Sub for LiteralKind {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        match self {
            LiteralKind::Integer(x) => match other {
                LiteralKind::Integer(y) => LiteralKind::Integer(x - y),
                LiteralKind::Real(y) => LiteralKind::Real((x as f32) - y),
            },
            LiteralKind::Real(x) => match other {
                LiteralKind::Integer(y) => LiteralKind::Real(x - (y as f32)),
                LiteralKind::Real(y) => LiteralKind::Real(x - y),
            }
        }
    }
}

impl Mul for LiteralKind {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match self {
            LiteralKind::Integer(x) => match other {
                LiteralKind::Integer(y) => LiteralKind::Integer(x * y),
                LiteralKind::Real(y) => LiteralKind::Real((x as f32) * y),
            },
            LiteralKind::Real(x) => match other {
                LiteralKind::Integer(y) => LiteralKind::Real(x * (y as f32)),
                LiteralKind::Real(y) => LiteralKind::Real(x * y),
            }
        }
    }
}

impl Neg for LiteralKind {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            LiteralKind::Integer(x) => LiteralKind::Integer(-x),
            LiteralKind::Real(x) => LiteralKind::Real(-x)
        }
    }
}

impl Div for LiteralKind {
    type Output = Result<Self, AstError>;

    fn div(self, other: Self) -> Self::Output {
        if other.is_zero() {
            return Err(AstError::DivisionByZero);
        }

        match self {
            LiteralKind::Integer(x) => match other {
                LiteralKind::Integer(y) => Ok(LiteralKind::Integer(x / y)),
                LiteralKind::Real(y) => Ok(LiteralKind::Real((x as f32) / y))
            },
            LiteralKind::Real(x) => match other {
                LiteralKind::Integer(y) => Ok(LiteralKind::Real(x / (y as f32))),
                LiteralKind::Real(y) => Ok(LiteralKind::Real(x / y))
            },
        }
    }
}

pub enum Expression {
    Binary(Operator, Box<Expression>, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(LiteralKind)
}

impl Expression {

    pub fn evaluate(&self) -> Result<LiteralKind, AstError> {
        match self {
            Expression::Literal(w) => Ok(w.clone()),
            Expression::Grouping(expr) => expr.evaluate(),
            Expression::Binary(op, left, right) => match op {
                Operator::Addition => Ok(left.evaluate()? + right.evaluate()?),
                Operator::Subtraction => Ok(left.evaluate()? - right.evaluate()?),
                Operator::Multiplication => Ok(left.evaluate()? * right.evaluate()?),
                Operator::Division => left.evaluate()? / right.evaluate()?,
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


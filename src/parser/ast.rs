#![allow(dead_code)] 

use std::fmt;

#[derive(Debug)]
pub enum AstError {
    DivisionByZero,
    IllegalUnaryOperator
}

impl fmt::Display for AstError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstError::DivisionByZero => write!(f, "Division by zero"),
            AstError::IllegalUnaryOperator => write!(f, "Illegal unary operator"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum LiteralKind {
    Integer(i32),
    Real(f32),
    Boolean(bool),
    Void
}

impl LiteralKind {
    pub fn is_zero(&self) -> bool {
        match self {
            LiteralKind::Integer(x) => *x == 0,
            LiteralKind::Real(x) => *x == 0.0,
            LiteralKind::Boolean(x) => *x == false,
            LiteralKind::Void => true,
        }
    }

    pub fn is_true(&self) -> bool {
        match self {
            LiteralKind::Boolean(x) => *x,
            LiteralKind::Real(x) => *x > 0.0,
            LiteralKind::Integer(x) => *x > 0,
            LiteralKind::Void => false
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            LiteralKind::Boolean(_) => 0,
            LiteralKind::Integer(_) => 1,
            LiteralKind::Real(_) => 2,
            LiteralKind::Void => 3,
        }
    }

}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralKind::Boolean(x) => write!(f, "{x}"),
            LiteralKind::Real(x) => write!(f, "{x}"),
            LiteralKind::Integer(x) => write!(f, "{x}"),
            LiteralKind::Void => write!(f, "Void"),
        }
    }
}

// Given a numeric operator this determines how two literals of any type can be combined
macro_rules! numeric_reduce {
    ($op:tt, $self:expr, $other:expr) => {
        match $self {
            LiteralKind::Integer(x) => match $other {
                LiteralKind::Integer(y) => LiteralKind::Integer(x $op y),
                LiteralKind::Real(y) => LiteralKind::Real((x as f32) $op y),
                LiteralKind::Boolean(y) => LiteralKind::Integer(x $op (y as i32)),
                LiteralKind::Void => LiteralKind::Integer(x),
            },
            LiteralKind::Real(x) => match $other {
                LiteralKind::Integer(y) => LiteralKind::Real(x $op (y as f32)),
                LiteralKind::Real(y) => LiteralKind::Real(x $op y),
                LiteralKind::Boolean(y) => LiteralKind::Real(x $op ((y as i32) as f32)),
                LiteralKind::Void => LiteralKind::Real(x),
            },
            LiteralKind::Boolean(x) => match $other {
                LiteralKind::Integer(y) => LiteralKind::Integer( (x as i32) $op y ),
                LiteralKind::Real(y) => LiteralKind::Real(((x as i32) as f32) $op y),
                LiteralKind::Boolean(y) => LiteralKind::Integer( (x as i32) $op (y as i32)  ),
                LiteralKind::Void => LiteralKind::Boolean(x),
            },
            LiteralKind::Void => match $other {
                LiteralKind::Integer(y) => LiteralKind::Integer(y),
                LiteralKind::Real(y) => LiteralKind::Real(y),
                LiteralKind::Boolean(y) => LiteralKind::Boolean(y),
                LiteralKind::Void => LiteralKind::Void,
            }
        }
    }
}

macro_rules! boolean_reduce {
    ($op:tt, $self:expr, $other:expr) => {
        match $self {
            LiteralKind::Integer(x) => match $other {
                LiteralKind::Integer(y) => LiteralKind::Boolean(x $op y),
                LiteralKind::Real(y) => LiteralKind::Boolean((x as f32) $op y),
                LiteralKind::Boolean(y) => LiteralKind::Boolean(x $op (y as i32)),
                LiteralKind::Void => LiteralKind::Void,
            },
            LiteralKind::Real(x) => match $other {
                LiteralKind::Integer(y) => LiteralKind::Boolean(x $op (y as f32)),
                LiteralKind::Real(y) => LiteralKind::Boolean(x $op y),
                LiteralKind::Boolean(y) => LiteralKind::Boolean(x $op ((y as i32) as f32)),
                LiteralKind::Void => LiteralKind::Void,
            },
            LiteralKind::Boolean(x) => match $other {
                LiteralKind::Integer(y) => LiteralKind::Boolean( (x as i32) $op y ),
                LiteralKind::Real(y) => LiteralKind::Boolean(((x as i32) as f32) $op y),
                LiteralKind::Boolean(y) => LiteralKind::Boolean(x $op y),
                LiteralKind::Void => LiteralKind::Void,
            },
            LiteralKind::Void => LiteralKind::Void
        }
    }
}

pub enum Expression {
    //Binary(Operator, Box<Expression>, Box<Expression>),
    BinaryAddition(Box<Expression>, Box<Expression>),
    BinarySubtraction(Box<Expression>, Box<Expression>),
    BinaryMultiplication(Box<Expression>, Box<Expression>),
    BinaryDivision(Box<Expression>, Box<Expression>),

//    Unary(Operator, Box<Expression>),
    UnaryNegation(Box<Expression>),
    UnaryAddition(Box<Expression>),

    GreaterThan(Box<Expression>, Box<Expression>),
    GreaterEqualThan(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    LessEqualThan(Box<Expression>, Box<Expression>),
    EqualTo(Box<Expression>, Box<Expression>),
    NotEqualTo(Box<Expression>, Box<Expression>),

    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),

    Grouping(Box<Expression>),
    Literal(LiteralKind)
}

impl Expression {

    pub fn evaluate(&self) -> Result<LiteralKind, AstError> {
        match self {
            Expression::Literal(w) => Ok(w.clone()),
            Expression::Grouping(expr) => expr.evaluate(),
            Expression::BinaryAddition(left, right) => Ok(numeric_reduce!(+, left.evaluate()?, right.evaluate()?)),
            Expression::BinarySubtraction(left, right) => Ok(numeric_reduce!(-, left.evaluate()?, right.evaluate()?)),
            Expression::BinaryMultiplication(left, right) => Ok(numeric_reduce!(*, left.evaluate()?, right.evaluate()?)),
            Expression::BinaryDivision(left, right) => {
                let right = right.evaluate()?;
                if right.is_zero() {
                    return Err(AstError::DivisionByZero);
                }
                return Ok(numeric_reduce!(/, left.evaluate()?, right));
            },

            Expression::GreaterThan(left, right) => Ok(boolean_reduce!(>, left.evaluate()?, right.evaluate()?)),
            Expression::GreaterEqualThan(left, right) => Ok(boolean_reduce!(>=, left.evaluate()?, right.evaluate()?)),
            Expression::LessThan(left, right) => Ok(boolean_reduce!(<, left.evaluate()?, right.evaluate()?)),
            Expression::LessEqualThan(left, right) => Ok(boolean_reduce!(<=, left.evaluate()?, right.evaluate()?)),

            Expression::EqualTo(left, right) => Ok(boolean_reduce!(==, left.evaluate()?, right.evaluate()?)),
            Expression::NotEqualTo(left, right) => Ok(boolean_reduce!(!=, left.evaluate()?, right.evaluate()?)),

            Expression::UnaryNegation(expr) => Ok(match expr.evaluate()? {
                LiteralKind::Integer(x) => LiteralKind::Integer(-x),
                LiteralKind::Real(x) => LiteralKind::Real(-x),
                LiteralKind::Boolean(x) => LiteralKind::Boolean(!x),
                LiteralKind::Void => LiteralKind::Void,
            }),
            Expression::UnaryAddition(expr) => Ok(expr.evaluate()?),

            Expression::Ternary(predicate, left, right) => 
                if predicate.evaluate()?.is_true() {
                    Ok(left.evaluate()?)
                }else {
                    Ok(right.evaluate()?)
                }
        }

    } 
}



impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Literal(x) => write!(f, "{x}"),
            Expression::Grouping(e) => write!(f, "({e})"),
            Expression::BinaryAddition(l, r) => write!(f, "(+ {l} {r})"),
            Expression::BinarySubtraction(l, r) => write!(f, "(- {l} {r})"),
            Expression::BinaryDivision(l, r) => write!(f, "(/ {l} {r})"),
            Expression::BinaryMultiplication(l, r) => write!(f, "(* {l} {r})"),

            Expression::GreaterThan(l, r) => write!(f, "(> {l} {r})"),
            Expression::GreaterEqualThan(l, r) => write!(f, "(>= {l} {r})"),
            Expression::LessThan(l, r) => write!(f, "(< {l} {r})"),
            Expression::LessEqualThan(l, r) => write!(f, "(<= {l} {r})"),

            Expression::EqualTo(l, r) => write!(f, "(== {l} {r})"),
            Expression::NotEqualTo(l, r) => write!(f, "(!= {l} {r})"),

            Expression::UnaryNegation(e) => write!(f, "(- {e})"),
            Expression::UnaryAddition(e) => write!(f, "(+ {e})"),

            Expression::Ternary(p, l, r) => write!(f, "(? {p} {l} : {r})"),

//            Expression::Binary(op, l, r) => write!(f, "({} {l} {r})", op.to_char()),
//            Expression::Unary(op, e) => write!(f, "({} {e})", op.to_char())
        }
    }
}


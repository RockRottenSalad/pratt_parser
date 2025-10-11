#![allow(dead_code)]

use crate::Environment;
use std::rc::Rc;
use std::fmt;
use std::result::Result;

#[derive(Debug)]
pub enum AstError {
    DivisionByZero,
    IllegalUnaryOperator,
    CannotImplicitityCastBoolToNumeric,
    CannotImplicitityCastNumericToBool,
    UnresolvedReference(Rc<str>),
}

impl fmt::Display for AstError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstError::DivisionByZero => write!(f, "Division by zero"),
            AstError::IllegalUnaryOperator => write!(f, "Illegal unary operator"),
            AstError::CannotImplicitityCastBoolToNumeric => write!(f, "Cannot implicity cast bool to numeric"),
            AstError::CannotImplicitityCastNumericToBool => write!(f, "Cannot implicity cast numeric to bool"),
            AstError::UnresolvedReference(x) => write!(f, "Unresolved reference('{x}')"),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum LiteralKind {
    Integer(i32),
    Real(f32),
    Boolean(bool),
    Void,
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
            LiteralKind::Void => false,
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

    pub fn is_integer(&self) -> bool {
        match self {
            LiteralKind::Integer(_) => true,
            _ => false
        }
    }
    pub fn is_real(&self) -> bool {
        match self {
            LiteralKind::Real(_) => true,
            _ => false
        }
    }
    pub fn is_boolean(&self) -> bool {
        match self {
            LiteralKind::Boolean(_) => true,
            _ => false
        }
    }

    pub fn default_integer() -> Self {
        LiteralKind::Integer(i32::default())
    }
    pub fn default_real() -> Self {
        LiteralKind::Real(f32::default())
    }
    pub fn default_boolean() -> Self {
        LiteralKind::Boolean(bool::default())
    }

    pub fn typecast(&self, other: &Self) -> LiteralKind {
        match other {
            LiteralKind::Void => LiteralKind::Void,
            LiteralKind::Boolean(_) => LiteralKind::Boolean(self.is_true()),
            LiteralKind::Integer(_) => match *self {
                LiteralKind::Real(x)    => LiteralKind::Integer(x as i32),
                LiteralKind::Boolean(x) => LiteralKind::Integer(x as i32),
                LiteralKind::Integer(x) => LiteralKind::Integer(x),
                LiteralKind::Void       => LiteralKind::Integer(0)
            },
            LiteralKind::Real(_) => match *self {
                LiteralKind::Real(x) => LiteralKind::Real(x),
                LiteralKind::Boolean(x) => LiteralKind::Real((x as i32) as f32),
                LiteralKind::Integer(x) => LiteralKind::Real(x as f32),
                LiteralKind::Void => LiteralKind::Real(0.0),
            },
        }
    }

    pub fn can_implicit_cast(&self, other: &Self) -> bool {
        match self {
            LiteralKind::Integer(_) => match other {
                LiteralKind::Integer(_) => true,
                LiteralKind::Real(_) => true,
                _ => false,
            },
            LiteralKind::Real(_) => match other {
                LiteralKind::Real(_) => true,
                LiteralKind::Integer(_) => true,
                _ => false,
            },
            LiteralKind::Boolean(_) => match other {
                LiteralKind::Boolean(_) => true,
                _ => false,
            },
            LiteralKind::Void => match other {
                LiteralKind::Void => true,
                _ => false,
            },
        }
    }

    pub fn is_same_type(&self, other: &Self) -> bool {
        match self {
            LiteralKind::Integer(_) => match other {
                LiteralKind::Integer(_) => true,
                _ => false,
            },
            LiteralKind::Real(_) => match other {
                LiteralKind::Real(_) => true,
                _ => false,
            },
            LiteralKind::Boolean(_) => match other {
                LiteralKind::Boolean(_) => true,
                _ => false,
            },
            LiteralKind::Void => match other {
                LiteralKind::Void => true,
                _ => false,
            },
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
                LiteralKind::Integer(y) => Ok(LiteralKind::Integer(x $op y)),
                LiteralKind::Real(y) => Ok(LiteralKind::Real((x as f32) $op y)),
                LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
                LiteralKind::Void => Ok(LiteralKind::Integer(x)),
            },
            LiteralKind::Real(x) => match $other {
                LiteralKind::Integer(y) => Ok(LiteralKind::Real(x $op (y as f32))),
                LiteralKind::Real(y) => Ok(LiteralKind::Real(x $op y)),
                LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
                LiteralKind::Void => Ok(LiteralKind::Real(x)),
            },
            LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
            LiteralKind::Void => Ok($other)
        }
    }
}

macro_rules! cmp_reduce {
    ($op:tt, $self:expr, $other:expr) => {
        match $self {
            LiteralKind::Integer(x) => match $other {
                LiteralKind::Integer(y) => Ok(LiteralKind::Boolean(x $op y)),
                LiteralKind::Real(y) => Ok(LiteralKind::Boolean((x as f32) $op y)),
                LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
                LiteralKind::Void => Ok(LiteralKind::Boolean(false)),
            },
            LiteralKind::Real(x) => match $other {
                LiteralKind::Integer(y) => Ok(LiteralKind::Boolean(x $op (y as f32))),
                LiteralKind::Real(y) => Ok(LiteralKind::Boolean(x $op y)),
                LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
                LiteralKind::Void => Ok(LiteralKind::Boolean(false)),
            },
            LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
            LiteralKind::Void => Ok(LiteralKind::Boolean(false))
        }
    }
}

macro_rules! boolean_reduce {
    ($op:tt, $self:expr, $other:expr) => {
        match $self {
            LiteralKind::Integer(_) => Err(AstError::CannotImplicitityCastNumericToBool),
            LiteralKind::Real(_) => Err(AstError::CannotImplicitityCastNumericToBool),
            LiteralKind::Void => Err(AstError::CannotImplicitityCastNumericToBool),

            LiteralKind::Boolean(x) => match $other {
                LiteralKind::Boolean(y) => Ok(LiteralKind::Boolean(x $op y)),

                LiteralKind::Integer(_) => Err(AstError::CannotImplicitityCastNumericToBool),
                LiteralKind::Real(_) => Err(AstError::CannotImplicitityCastNumericToBool),
                LiteralKind::Void => Err(AstError::CannotImplicitityCastNumericToBool),
            },
        }
    }
}

#[derive(Debug)]
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

    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),

    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),

    Grouping(Box<Expression>),

    Literal(LiteralKind),
    Typecast(Box<Expression>, LiteralKind),

    Reference(Rc<str>),
}

impl Expression {
    pub fn evaluate(&self, env: Option<&Environment>) -> Result<LiteralKind, AstError> {
        match self {
            Expression::Literal(w) => Ok(w.clone()),
            Expression::Grouping(expr) => expr.evaluate(env),
            Expression::BinaryAddition(left, right) => {
                numeric_reduce!(+, left.evaluate(env)?, right.evaluate(env)?)
            }
            Expression::BinarySubtraction(left, right) => {
                numeric_reduce!(-, left.evaluate(env)?, right.evaluate(env)?)
            }
            Expression::BinaryMultiplication(left, right) => {
                numeric_reduce!(*, left.evaluate(env)?, right.evaluate(env)?)
            }
            Expression::BinaryDivision(left, right) => {
                let right = right.evaluate(env)?;
                if right.is_zero() {
                    return Err(AstError::DivisionByZero);
                }
                return numeric_reduce!(/, left.evaluate(env)?, right);
            }

            Expression::GreaterThan(left, right) => cmp_reduce!(>, left.evaluate(env)?, right.evaluate(env)?),
            Expression::GreaterEqualThan(left, right) => cmp_reduce!(>=, left.evaluate(env)?, right.evaluate(env)?),
            Expression::LessThan(left, right) => cmp_reduce!(<, left.evaluate(env)?, right.evaluate(env)?),
            Expression::LessEqualThan(left, right) => cmp_reduce!(<=, left.evaluate(env)?, right.evaluate(env)?),

            Expression::EqualTo(left, right) => cmp_reduce!(==, left.evaluate(env)?, right.evaluate(env)?),
            Expression::NotEqualTo(left, right) => cmp_reduce!(!=, left.evaluate(env)?, right.evaluate(env)?),

            Expression::And(left, right) => boolean_reduce!(&&, left.evaluate(env)?, right.evaluate(env)?),
            Expression::Or(left, right) => boolean_reduce!(||, left.evaluate(env)?, right.evaluate(env)?),

            Expression::UnaryNegation(expr) => Ok(match expr.evaluate(env)? {
                LiteralKind::Integer(x) => LiteralKind::Integer(-x),
                LiteralKind::Real(x) => LiteralKind::Real(-x),
                LiteralKind::Boolean(x) => LiteralKind::Boolean(!x),
                LiteralKind::Void => LiteralKind::Void,
            }),
            Expression::UnaryAddition(expr) => Ok(expr.evaluate(env)?),

            Expression::Ternary(predicate, left, right) => {

                let predicate = predicate.evaluate(env)?;
                if !predicate.is_boolean() {
                    return Err(AstError::CannotImplicitityCastNumericToBool);
                }

                if predicate.is_true() {
                    Ok(left.evaluate(env)?)
                } else {
                    Ok(right.evaluate(env)?)
                }
            },

            Expression::Typecast(expr, kind) => Ok(expr.evaluate(env)?.typecast(kind)),

            Expression::Reference(var) => match env {
                Some(env) => match env.get_variable(var) {
                    Some(v) => Ok(v),
                    None => Err(AstError::UnresolvedReference(Rc::clone(var))),
                }
                None => Err(AstError::UnresolvedReference(Rc::clone(var)))
        }
//            Expression::Reference(x) =>  x.evaluate(),
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

            Expression::Reference(s) => write!(f, "(ref {s})"),
            Expression::Typecast(e, t) => write!(f, "(as {e} {t})"),

            Expression::And(l, r) => write!(f, "(and {l} {r})"),
            Expression::Or(l, r) => write!(f, "(or {l} {r})"),
//            Expression::Reference(s) => write!(f, "(ref {s})"),
            //            Expression::Binary(op, l, r) => write!(f, "({} {l} {r})", op.to_char()),
            //            Expression::Unary(op, e) => write!(f, "({} {e})", op.to_char())
        }
    }
}


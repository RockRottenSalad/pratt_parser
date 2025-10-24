#![allow(dead_code)]

use crate::State;
use std::fmt;
use std::rc::Rc;
use std::result::Result;

#[derive(Debug)]
pub enum AstError {
    DivisionByZero,
    IllegalUnaryOperator,
    CannotImplicitityCastBoolToNumeric,
    CannotImplicitityCastNumericToBool,
    CannotImplicitityCastChar,
    OperatorNotSupportedWithArray, // todo - include op in err
    UnresolvedReference(Rc<str>),
    IncorrectNumberOfArguments(usize, usize),
    IncorrectArgumentType(LiteralKind, LiteralKind, usize),
}

impl fmt::Display for AstError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstError::DivisionByZero => write!(f, "Division by zero"),
            AstError::IllegalUnaryOperator => write!(f, "Illegal unary operator"),
            AstError::CannotImplicitityCastBoolToNumeric => {
                write!(f, "Cannot implicity cast bool to numeric")
            }
            AstError::CannotImplicitityCastNumericToBool => {
                write!(f, "Cannot implicity cast numeric to bool")
            }
            AstError::UnresolvedReference(x) => write!(f, "Unresolved reference('{x}')"),
            AstError::IncorrectNumberOfArguments(e, g) => write!(
                f,
                "Incorrect number of arguments. Expected '{e}', got '{g}'"
            ),
            AstError::IncorrectArgumentType(e, g, i) => write!(
                f,
                "Incorrect argument type. Expected '{e}', got '{g}' at argument '{i}"
            ),
            AstError::CannotImplicitityCastChar => write!(
                f,
                "Cannot implicity cast a char to another type"
            ),
            AstError::OperatorNotSupportedWithArray => write!(
                f,
                "Operator not supported with array"
            )
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum LiteralKind {
    Integer(i32),
    Real(f32),
    Boolean(bool),
    Char(char),
    Array(Vec<LiteralKind>),
    Void,
}

impl LiteralKind {
    pub fn is_zero(&self) -> bool {
        match self {
            LiteralKind::Integer(x) => *x == 0,
            LiteralKind::Real(x) => *x == 0.0,
            LiteralKind::Boolean(x) => *x == false,
            LiteralKind::Char(x) => *x == '\0',
            LiteralKind::Array(x) => x.len() == 0,
            LiteralKind::Void => true,
        }
    }

    pub fn is_true(&self) -> bool {
        match self {
            LiteralKind::Boolean(x) => *x,
            _ => false
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            LiteralKind::Boolean(_) => 0,
            LiteralKind::Integer(_) => 1,
            LiteralKind::Array(_) => 1,
            LiteralKind::Char(_) => 1,
            LiteralKind::Real(_) => 2,
            LiteralKind::Void => 3,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            LiteralKind::Integer(_) => true,
            _ => false,
        }
    }
    pub fn is_real(&self) -> bool {
        match self {
            LiteralKind::Real(_) => true,
            _ => false,
        }
    }
    pub fn is_boolean(&self) -> bool {
        match self {
            LiteralKind::Boolean(_) => true,
            _ => false,
        }
    }
    pub fn is_char(&self) -> bool {
        match self {
            LiteralKind::Char(_) => true,
            _ => false,
        }
    }
    pub fn is_array(&self) -> bool {
        match self {
            LiteralKind::Array(_) => true,
            _ => false,
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
    pub fn default_char(&self) -> Self {
        LiteralKind::Char(char::default())
    }
    pub fn default_array(&self) -> Self {
        LiteralKind::Array(Vec::new())
    }

    pub fn typecast(&self, other: &Self) -> LiteralKind {
        match other {
            LiteralKind::Void => LiteralKind::Void,
            LiteralKind::Boolean(_) => LiteralKind::Boolean(self.is_true()),
            LiteralKind::Integer(_) => match *self {
                LiteralKind::Real(x) => LiteralKind::Integer(x as i32),
                LiteralKind::Boolean(x) => LiteralKind::Integer(x as i32),
                LiteralKind::Integer(x) => LiteralKind::Integer(x),
                LiteralKind::Void => LiteralKind::Integer(0),
                LiteralKind::Char(x) => LiteralKind::Integer(x as i32),
                LiteralKind::Array(_) => panic!("TODO: Don't panic")
            },
            LiteralKind::Real(_) => match *self {
                LiteralKind::Real(x) => LiteralKind::Real(x),
                LiteralKind::Boolean(x) => LiteralKind::Real((x as i32) as f32),
                LiteralKind::Char(x) => LiteralKind::Real((x as i32) as f32),
                LiteralKind::Integer(x) => LiteralKind::Real(x as f32),
                LiteralKind::Void => LiteralKind::Real(0.0),
                LiteralKind::Array(_) => panic!("TODO: Don't panic")
            },
            LiteralKind::Char(_) => match *self {
                LiteralKind::Real(_) => LiteralKind::Void,
                LiteralKind::Void => LiteralKind::Void,
                LiteralKind::Boolean(_) => LiteralKind::Void,
                LiteralKind::Char(x) => LiteralKind::Char(x),
                LiteralKind::Integer(x) => LiteralKind::Char(char::from_u32(x as u32).unwrap()),
                LiteralKind::Array(_) => panic!("TODO: Don't panic")
            },
            LiteralKind::Array(_) => panic!("TODO: Don't panic")
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
            LiteralKind::Char(_) => match other {
                LiteralKind::Char(_) => true,
                _ => false
            },
            LiteralKind::Array(_) => false,
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
            LiteralKind::Char(_) => match other {
                LiteralKind::Char(_) => true,
                _ => false,
            },
            LiteralKind::Array(_) => match other {
                LiteralKind::Array(_) => true,
                _ => false,
            },
        }
    }

    pub fn type_as_str(&self) -> &str {
        match self {
            LiteralKind::Boolean(_) => "Boolean",
            LiteralKind::Integer(_) => "Integer",
            LiteralKind::Real(_) => "Real",
            LiteralKind::Char(_) => "Char",
            LiteralKind::Array(_) => "Array",
            LiteralKind::Void => "Void",
        }
    }
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralKind::Boolean(x) => write!(f, "{x}"),
            LiteralKind::Real(x) => write!(f, "{x}"),
            LiteralKind::Integer(x) => write!(f, "{x}"),
            LiteralKind::Char(x) => write!(f, "{x}"),
            LiteralKind::Array(x) => write!(f, "{:?}", x),
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
                LiteralKind::Char(_) => Err(AstError::CannotImplicitityCastChar),
                LiteralKind::Void => Ok(LiteralKind::Integer(x)),
                LiteralKind::Array(_) => Err(AstError::OperatorNotSupportedWithArray)
            },
            LiteralKind::Real(x) => match $other {
                LiteralKind::Integer(y) => Ok(LiteralKind::Real(x $op (y as f32))),
                LiteralKind::Real(y) => Ok(LiteralKind::Real(x $op y)),
                LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
                LiteralKind::Char(_) => Err(AstError::CannotImplicitityCastChar),
                LiteralKind::Void => Ok(LiteralKind::Real(x)),
                LiteralKind::Array(_) => Err(AstError::OperatorNotSupportedWithArray)
            },
            LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
            LiteralKind::Void => Ok($other),
            LiteralKind::Char(_) => panic!("Not implemented yet"),
            LiteralKind::Array(_) => Err(AstError::OperatorNotSupportedWithArray)
        }
    }
}

macro_rules! cmp_reduce {
    ($op:tt, $self:expr, $other:expr) => {
        match $self {
            LiteralKind::Integer(x) => match $other {
                LiteralKind::Integer(y) => Ok(LiteralKind::Boolean(x $op y)),
                LiteralKind::Char(y) => Ok(LiteralKind::Boolean(x $op (y as i32))),
                LiteralKind::Real(y) => Ok(LiteralKind::Boolean((x as f32) $op y)),
                LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
                LiteralKind::Void => Ok(LiteralKind::Boolean(false)),
                LiteralKind::Array(_) => Err(AstError::OperatorNotSupportedWithArray)
            },
            LiteralKind::Real(x) => match $other {
                LiteralKind::Integer(y) => Ok(LiteralKind::Boolean(x $op (y as f32))),
                LiteralKind::Real(y) => Ok(LiteralKind::Boolean(x $op y)),
                LiteralKind::Char(y) => Ok(LiteralKind::Boolean(x $op (y as i32) as f32)),
                LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
                LiteralKind::Void => Ok(LiteralKind::Boolean(false)),
                LiteralKind::Array(_) => Err(AstError::OperatorNotSupportedWithArray)
            },
            LiteralKind::Char(x) => match $other {
                LiteralKind::Char(y) => Ok(LiteralKind::Boolean(x $op y)),
                _ => panic!("Not implemented")
            },
            LiteralKind::Boolean(_) => Err(AstError::CannotImplicitityCastBoolToNumeric),
            LiteralKind::Void => Ok(LiteralKind::Boolean(false)),
            LiteralKind::Array(_) => Err(AstError::OperatorNotSupportedWithArray),
        }
    }
}

macro_rules! boolean_reduce {
    ($op:tt, $self:expr, $other:expr) => {
        match $self {
            LiteralKind::Integer(_) => Err(AstError::CannotImplicitityCastNumericToBool),
            LiteralKind::Real(_) => Err(AstError::CannotImplicitityCastNumericToBool),
            LiteralKind::Char(_) => Err(AstError::CannotImplicitityCastChar),
            LiteralKind::Void => Err(AstError::CannotImplicitityCastNumericToBool),
            LiteralKind::Array(_) => Err(AstError::OperatorNotSupportedWithArray),

            LiteralKind::Boolean(x) => match $other {
                LiteralKind::Boolean(y) => Ok(LiteralKind::Boolean(x $op y)),

                LiteralKind::Integer(_) => Err(AstError::CannotImplicitityCastNumericToBool),
                LiteralKind::Real(_) => Err(AstError::CannotImplicitityCastNumericToBool),
                LiteralKind::Char(_) => Err(AstError::CannotImplicitityCastChar),
                LiteralKind::Void => Err(AstError::CannotImplicitityCastNumericToBool),
                LiteralKind::Array(_) => Err(AstError::OperatorNotSupportedWithArray),
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
    BinaryModulo(Box<Expression>, Box<Expression>),

    //    Unary(Operator, Box<Expression>),
    UnaryNumericNegation(Box<Expression>),
    UnaryBooleanNegation(Box<Expression>),
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

    FunctionCall(Rc<str>, Vec<Box<Expression>>),
}

impl Expression {
    pub fn evaluate(&self, state: Option<*mut State>) -> Result<LiteralKind, AstError> {
        match self {
            Expression::Literal(w) => Ok(w.clone()),
            Expression::Grouping(expr) => expr.evaluate(state),
            Expression::BinaryAddition(left, right) => {
                numeric_reduce!(+, left.evaluate(state)?, right.evaluate(state)?)
            }
            Expression::BinarySubtraction(left, right) => {
                numeric_reduce!(-, left.evaluate(state)?, right.evaluate(state)?)
            }
            Expression::BinaryMultiplication(left, right) => {
                numeric_reduce!(*, left.evaluate(state)?, right.evaluate(state)?)
            }
            Expression::BinaryDivision(left, right) => {
                let right = right.evaluate(state)?;
                if right.is_zero() {
                    return Err(AstError::DivisionByZero);
                }
                return numeric_reduce!(/, left.evaluate(state)?, right);
            }
            Expression::BinaryModulo(left, right) => {
                numeric_reduce!(%, left.evaluate(state)?, right.evaluate(state)? )
            }
            Expression::GreaterThan(left, right) => {
                cmp_reduce!(>, left.evaluate(state)?, right.evaluate(state)?)
            }
            Expression::GreaterEqualThan(left, right) => {
                cmp_reduce!(>=, left.evaluate(state)?, right.evaluate(state)?)
            }
            Expression::LessThan(left, right) => {
                cmp_reduce!(<, left.evaluate(state)?, right.evaluate(state)?)
            }
            Expression::LessEqualThan(left, right) => {
                cmp_reduce!(<=, left.evaluate(state)?, right.evaluate(state)?)
            }

            Expression::EqualTo(left, right) => {
                cmp_reduce!(==, left.evaluate(state)?, right.evaluate(state)?)
            }
            Expression::NotEqualTo(left, right) => {
                cmp_reduce!(!=, left.evaluate(state)?, right.evaluate(state)?)
            }

            Expression::And(left, right) => {
                boolean_reduce!(&&, left.evaluate(state)?, right.evaluate(state)?)
            }
            Expression::Or(left, right) => {
                boolean_reduce!(||, left.evaluate(state)?, right.evaluate(state)?)
            }

            Expression::UnaryNumericNegation(expr) => Ok(match expr.evaluate(state)? {
                LiteralKind::Integer(x) => LiteralKind::Integer(-x),
                LiteralKind::Real(x) => LiteralKind::Real(-x),
                _ => {return Err(AstError::CannotImplicitityCastBoolToNumeric)}
            }),
            Expression::UnaryBooleanNegation(expr) => Ok(match expr.evaluate(state)? {
                LiteralKind::Boolean(x) => LiteralKind::Boolean(!x),
                _ => {return Err(AstError::CannotImplicitityCastNumericToBool)}
            }),
            Expression::UnaryAddition(expr) => Ok(expr.evaluate(state)?),

            Expression::Ternary(predicate, left, right) => {
                let predicate = predicate.evaluate(state)?;
                if !predicate.is_boolean() {
                    return Err(AstError::CannotImplicitityCastNumericToBool);
                }

                if predicate.is_true() {
                    Ok(left.evaluate(state)?)
                } else {
                    Ok(right.evaluate(state)?)
                }
            }

            Expression::Typecast(expr, kind) => Ok(expr.evaluate(state)?.typecast(kind)),

            Expression::Reference(var) => match state {
                Some(state) => unsafe {
                    match (*state).borrow_env().get_variable(var) {
                        Some(v) => Ok(v),
                        None => Err(AstError::UnresolvedReference(Rc::clone(var))),
                    }
                },
                None => Err(AstError::UnresolvedReference(Rc::clone(var))),
            },

            Expression::FunctionCall(f_name, xs) => match state {
                Some(state) => unsafe {
                    match (*state).borrow_env().get_function(f_name) {
                        // TODO fix possible panic
                        Some(f) => match f.evaluate(xs, state) {
                            Ok(v) => Ok(v),
                            Err(_) => Err(AstError::UnresolvedReference(Rc::clone(f_name)))
                        }
                        None => Err(AstError::UnresolvedReference(Rc::clone(f_name))),
                    }
                },
                None => Err(AstError::UnresolvedReference(Rc::clone(f_name))),
            },
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
            Expression::BinaryModulo(l, r) => write!(f, "(% {l} {r})"),

            Expression::GreaterThan(l, r) => write!(f, "(> {l} {r})"),
            Expression::GreaterEqualThan(l, r) => write!(f, "(>= {l} {r})"),
            Expression::LessThan(l, r) => write!(f, "(< {l} {r})"),
            Expression::LessEqualThan(l, r) => write!(f, "(<= {l} {r})"),

            Expression::EqualTo(l, r) => write!(f, "(== {l} {r})"),
            Expression::NotEqualTo(l, r) => write!(f, "(!= {l} {r})"),

            Expression::UnaryNumericNegation(e) => write!(f, "(- {e})"),
            Expression::UnaryBooleanNegation(e) => write!(f, "(! {e})"),
            Expression::UnaryAddition(e) => write!(f, "(+ {e})"),

            Expression::Ternary(p, l, r) => write!(f, "(? {p} {l} : {r})"),

            Expression::Reference(s) => write!(f, "(ref {s})"),
            Expression::Typecast(e, t) => write!(f, "(as {e} {t})"),

            Expression::And(l, r) => write!(f, "(and {l} {r})"),
            Expression::Or(l, r) => write!(f, "(or {l} {r})"),
            Expression::FunctionCall(y, xs) => write!(f, "(() {y} {:?})", xs),
        }
    }
}

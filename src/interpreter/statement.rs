#![allow(dead_code)]

use crate::ast::{Expression, LiteralKind};
use crate::interpreter::interpreter::Environment;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub enum Statement {
    Print(Box<Expression>),
    Assignment(Rc<str>, Box<Expression>),
}

impl Statement {
    pub fn execute<'a>(&'a self, env: &mut Environment<'a>) {
        match self {
            Statement::Print(expr) => println!("{}", expr.evaluate().unwrap_or_else(|_| LiteralKind::Integer(0))), // <- TODO: Fix this
            Statement::Assignment(var, expr) => match expr.evaluate() {
                Ok(x) => env.declare_variable(var, x),
                Err(..) => env.declare_variable(var, LiteralKind::Integer(0)), // <- TODO: Fix this
            },
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Print(expr) => write!(f, "print ({expr})"),
            Statement::Assignment(var, expr) => write!(f, "{var} = ({expr})"),
        }
    }
}

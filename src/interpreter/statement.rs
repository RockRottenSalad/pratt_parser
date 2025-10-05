#![allow(dead_code)]

use crate::ast::{Expression, AstError};
use crate::interpreter::interpreter::Environment;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub enum Statement {
    Print(Box<Expression>),
    Assignment(Rc<str>, Box<Expression>),
}

impl Statement {
    pub fn execute<'a>(&'a self, env: &mut Environment) -> Result<(), AstError>{
        match self {
            Statement::Print(expr) => match expr.evaluate(Some(env)) {
                Ok(v) => println!("{v}"),
                Err(e) => return Err(e),
            },
            Statement::Assignment(var, expr) => match expr.evaluate(Some(env)) {
                Ok(x) => env.declare_variable(&Rc::clone(var), x),
                Err(e) => return Err(e),
            },
        };

        Ok(())
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

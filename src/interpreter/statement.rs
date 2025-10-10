#![allow(dead_code)]

use crate::ast::{AstError, Expression};
use crate::interpreter::interpreter::Environment;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub enum Statement {
    Print(Box<Expression>),
    Assignment(Rc<str>, Box<Expression>),
    If(Box<Expression>, Box<Statement>),
    IfElse(Box<Expression>, Box<Statement>, Box<Statement>),
    Block(Vec<Box<Statement>>),
}

impl Statement {
    pub fn execute<'a>(&'a self, mut env: Box<Environment>) -> Result<Box<Environment>, (Box<Environment>, AstError)> {
        match self {
            Statement::Print(expr) => match expr.evaluate(Some(&mut env)) {
                Ok(v) => println!("{v}"),
                Err(e) => return Err((env, e)),
            },
            Statement::Assignment(var, expr) => match expr.evaluate(Some(&mut env)) {
                Ok(x) => env.declare_variable(&Rc::clone(var), x),
                Err(e) => return Err((env, e)),
            },
            Statement::If(cond, stm) => match cond.evaluate(Some(&mut env)) {
                Ok(v) => {
                    if v.is_true() {
                        return stm.execute(env);
                    } else {
                        return Ok(env);
                    }
                }
                Err(e) => return Err((env, e)),
            },
            Statement::IfElse(cond, a, b) => match cond.evaluate(Some(&mut env)) {
                Ok(v) => {
                    if v.is_true() {
                        return a.execute(env);
                    } else {
                        return b.execute(env);
                    }
                }
                Err(e) => return Err((env, e)),
            },
            Statement::Block(stms) => {
                env = Environment::new(Some(env));
                for stm in stms {
                    match stm.execute(env) {
                        Ok(new_env) => env = new_env,
                        Err(e) => return Err(e),
                    }
                }
                if env.has_parent() {
                    env = env.parent();
                }
            }
        };

        Ok(env)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Print(expr) => write!(f, "print ({expr})"),
            Statement::Assignment(var, expr) => write!(f, "{var} = ({expr})"),
            Statement::If(cond, stm) => write!(f, "if ({cond}) {{{stm}}}"),
            Statement::IfElse(cond, a, b) => write!(f, "if ({cond}) {{{a}}} else {{{b}}}"),
            Statement::Block(stms) => write!(f, "{{ {:?} }}", stms),
        }
    }
}

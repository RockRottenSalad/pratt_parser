#![allow(dead_code)]

use crate::function::Function;
use crate::InterpreterError;
use crate::ast::{Expression};
use crate::interpreter::interpreter::State;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub enum Statement {
    Print(Box<Expression>),
    Assignment(Rc<str>, Box<Expression>),
    FunctionAssignment(Rc<str>, Rc<Function>),
    If(Box<Expression>, Box<Statement>),
    IfElse(Box<Expression>, Box<Statement>, Box<Statement>),
    Block(Vec<Box<Statement>>),
}

impl Statement {
    pub fn execute<'a>(&'a self, state: &mut State) -> Result<(), InterpreterError> {
        match self {
            Statement::Print(expr) => match expr.evaluate(Some(state.env())) {
                Ok(v) => println!("{v}"),
                Err(e) => return Err(InterpreterError::Ast(e)),
            },
            Statement::Assignment(var, expr) => match expr.evaluate(Some(state.env())) {
                Ok(x) => state.env().declare_variable(&Rc::clone(var), x),
                Err(e) => return Err(InterpreterError::Ast(e)),
            },
            Statement::FunctionAssignment(var, func) => {
                state.env().declare_function(var, func.clone())
            },
            Statement::If(cond, stm) => match cond.evaluate(Some(state.env())) {
                Ok(v) => {
                    if v.is_true() {
                        return stm.execute(state);
                    } else {
                        return Ok(());
                    }
                }
                Err(e) => return Err(InterpreterError::Ast(e)),
            },
            Statement::IfElse(cond, a, b) => match cond.evaluate(Some(state.env())) {
                Ok(v) => {
                    if v.is_true() {
                        return a.execute(state);
                    } else {
                        return b.execute(state);
                    }
                }
                Err(e) => return Err(InterpreterError::Ast(e)),
            },
            Statement::Block(stms) => {
                state.enter_scope();
                for stm in stms {
                    match stm.execute(state) {
                        Ok(()) => {},
                        Err(e) => { state.leave_scope()?; return Err(e); },
                    }
                }
                state.leave_scope()?;
            }
        };

        Ok(())
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Print(expr) => write!(f, "print ({expr})"),
            Statement::Assignment(var, expr) => write!(f, "{var} = ({expr})"),
            Statement::FunctionAssignment(var, _expr) => write!(f, "{var} = ..."),
            Statement::If(cond, stm) => write!(f, "if ({cond}) {{{stm}}}"),
            Statement::IfElse(cond, a, b) => write!(f, "if ({cond}) {{{a}}} else {{{b}}}"),
            Statement::Block(stms) => write!(f, "{{ {:?} }}", stms),
        }
    }
}

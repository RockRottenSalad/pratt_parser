#![allow(dead_code)]

use crate::InterpreterError;
use crate::ast::{AstError, Expression};
use crate::function::Function;
use crate::interpreter::interpreter::State;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub enum Statement {
    Print(Box<Expression>),

    Assignment(Rc<str>, Box<Expression>),
    Reassignment(Rc<str>, Box<Expression>),

    FunctionAssignment(Rc<str>, Rc<Function>),
    FunctionReassignment(Rc<str>, Rc<Function>),

    If(Box<Expression>, Box<Statement>),
    IfElse(Box<Expression>, Box<Statement>, Box<Statement>),
    Block(Vec<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
}

impl Statement {
    pub fn execute<'a>(&'a self, state: &mut State) -> Result<(), InterpreterError> {
        match self {
            Statement::While(cond, stm) => {
                while match cond.evaluate(Some(state.env())) {
                    Ok(v) => {
                        if !v.is_boolean() {
                            return Err(InterpreterError::Ast(
                                AstError::CannotImplicitityCastNumericToBool,
                            ));
                        } else {
                            v.is_true()
                        }
                    }
                    Err(e) => return Err(InterpreterError::Ast(e)),
                } {
                    stm.execute(state)?
                }
            }
            Statement::Print(expr) => match expr.evaluate(Some(state.env())) {
                Ok(v) => println!("{v}"),
                Err(e) => return Err(InterpreterError::Ast(e)),
            },
            Statement::Assignment(var, expr) => match expr.evaluate(Some(state.env())) {
                Ok(x) => state.borrow_env().declare_variable(&Rc::clone(var), x),
                Err(e) => return Err(InterpreterError::Ast(e)),
            },
            Statement::Reassignment(var, expr) => match expr.evaluate(Some(state.env())) {
                Ok(x) => {
                    if !state.borrow_env().reassign_variable(&Rc::clone(var), x) {
                        return Err(InterpreterError::Ast(AstError::UnresolvedReference(
                            Rc::clone(&var),
                        )));
                    }
                }
                Err(e) => return Err(InterpreterError::Ast(e)),
            },
            Statement::FunctionAssignment(var, func) => {
                state.borrow_env().declare_function(var, func.clone())
            }
            Statement::FunctionReassignment(var, func) => {
                if !state.borrow_env().reassign_function(var, func.clone()) {
                    return Err(InterpreterError::Ast(AstError::UnresolvedReference(
                        Rc::clone(&var),
                    )));
                }
            }
            Statement::If(cond, stm) => match cond.evaluate(Some(state.env())) {
                Ok(v) => {
                    if !v.is_boolean() {
                        return Err(InterpreterError::Ast(
                            AstError::CannotImplicitityCastNumericToBool,
                        ));
                    } else {
                        if v.is_true() {
                            return stm.execute(state);
                        } else {
                            return Ok(());
                        }
                    }
                }
                Err(e) => return Err(InterpreterError::Ast(e)),
            },
            Statement::IfElse(cond, a, b) => match cond.evaluate(Some(state.env())) {
                Ok(v) => if !v.is_boolean() {
                    return Err(InterpreterError::Ast(
                        AstError::CannotImplicitityCastNumericToBool,
                    ));
                }else {
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
                        Ok(()) => {}
                        Err(e) => {
                            state.leave_scope()?;
                            return Err(e);
                        }
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
            Statement::Assignment(var, expr) => write!(f, "let {var} = ({expr})"),
            Statement::Reassignment(var, expr) => write!(f, "{var} = ({expr})"),
            Statement::FunctionAssignment(var, expr) => write!(f, "let {var} = {expr}"),
            Statement::FunctionReassignment(var, expr) => write!(f, "{var} = {expr}"),
            Statement::If(cond, stm) => write!(f, "if ({cond}) {{{stm}}}"),
            Statement::IfElse(cond, a, b) => write!(f, "if ({cond}) {{{a}}} else {{{b}}}"),
            Statement::Block(stms) => write!(f, "{{ {:?} }}", stms),
            Statement::While(cond, stm) => write!(f, "while ({cond}) {stm}"),
        }
    }
}

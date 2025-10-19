#![allow(dead_code)]

// Maybe move this to ast.rs?

use crate::InterpreterError;
use crate::interpreter::statement::Statement;
use crate::State;
use crate::ast::{Expression, LiteralKind, AstError};
use std::rc::Rc;
use std::fmt;

#[derive(Debug)]
pub struct Argument {
    name: Rc<str>,
    kind: LiteralKind,
}

impl Argument {
    pub fn new(name: Rc<str>, kind: LiteralKind) -> Self {
        Argument { name, kind }
    }
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.name, self.kind.type_as_str())
    }
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<Argument>,
    pub body: Box<Statement>, // TODO - should allow block statement in the future
}

impl Function {

    pub fn evaluate(&self, args: &Vec<Box<Expression>>, state: *mut State) -> Result<LiteralKind, InterpreterError> {

        if args.len() != self.parameters.len() {
            return Err(
                InterpreterError::Ast(
                AstError::IncorrectNumberOfArguments(self.parameters.len(), args.len())
            ))
        }

        unsafe { (*state).enter_scope() };

        for (i, v) in args.iter().map(|x| x.evaluate(Some(state))).enumerate() {
            let value = match v {
                Ok(v) => v,
                Err(e) => return Err(InterpreterError::Ast(e))
            };

            if !value.is_same_type(&self.parameters[i].kind) {
                unsafe { (*state).leave_scope()? };

                return Err(
                    InterpreterError::Ast(
                    AstError::IncorrectArgumentType(self.parameters[i].kind, value, i)
                ))
            }

            unsafe {
                (*state).borrow_env_mut().declare_variable(&self.parameters[i].name, value);
            }
        }

        let ret = self.body.execute(state);

        unsafe { (*state).leave_scope()? };

        ret
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} -> {}", self.parameters, self.body)
    }
}



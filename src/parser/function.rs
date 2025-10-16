#![allow(dead_code)]

// Maybe move this to ast.rs?

use crate::Environment;
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
    pub body: Box<Expression>, // TODO - should allow block statement in the future
}

impl Function {

    pub fn evaluate(&self, args: &Vec<Box<Expression>>, env: *mut Environment) -> Result<LiteralKind, AstError> {

        if args.len() != self.parameters.len() {
            return Err(AstError::IncorrectNumberOfArguments(self.parameters.len(), args.len()))
        }

        let mut local_env = Environment::new_non_owning(env);

        for (i, v) in args.iter().map(|x| x.evaluate(Some(env))).enumerate() {
            let value = v?;

            if !value.is_same_type(&self.parameters[i].kind) {
                return Err(AstError::IncorrectArgumentType(self.parameters[i].kind, value, i))
            }

            local_env.declare_variable(&self.parameters[i].name, value)
        }

        self.body.evaluate(Some(&mut *local_env))
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} -> {}", self.parameters, self.body)
    }
}



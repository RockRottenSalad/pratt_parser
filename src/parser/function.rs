#![allow(dead_code)]

// Maybe move this to ast.rs?

use crate::Environment;
use crate::ast::{Expression, LiteralKind, AstError};
use std::rc::Rc;

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

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<Argument>,
    pub body: Box<Expression>, // TODO - should allow block statement in the future
}

impl Function {

    pub fn evaluate(&self, args: &Vec<Box<Expression>>) -> Result<LiteralKind, AstError> {

        if args.len() != self.parameters.len() {
            return Err(AstError::IncorrectNumberOfArguments(self.parameters.len(), args.len()))
        }

        // The actual env is totally ignored right now
        let mut local_env = Environment::new(None);

        for (i, v) in args.iter().map(|x| x.evaluate(None)).enumerate() {
            let value = v?;

            if !value.is_same_type(&self.parameters[i].kind) {
                panic!("WIP - Function argument and parameter type mismatch. Expected {}, got {} at arg {}", self.parameters[i].kind, args[i], i)
            }

            local_env.declare_variable(&self.parameters[i].name, value)
        }

        self.body.evaluate(Some(&local_env))
    }
}



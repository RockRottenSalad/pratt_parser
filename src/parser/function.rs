#![allow(dead_code)]

// Maybe move this to ast.rs?

use crate::State;
use crate::Environment;
use crate::ast::{Expression, LiteralKind, AstError};
use std::rc::Rc;

#[derive(Debug)]
pub struct Argument {
    pub name: Rc<str>,
    pub kind: LiteralKind,
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<Argument>,
    pub body: Box<Expression>, // TODO - should allow block statement in the future
    pub return_value: LiteralKind
}

impl Function {

    pub fn evaluate(&self, args: &Vec<LiteralKind>) -> Result<LiteralKind, AstError> {

        if args.len() != self.parameters.len() {
            panic!("WIP - Function arguments and parameter count mismatch. Expected {}, got {}", self.parameters.len(), args.len())
        }

        for i in 0..args.len() {
            if !args[i].is_same_type(&self.parameters[i].kind) {
                panic!("WIP - Function argument and parameter type mismatch. Expected {}, got {} at arg {}", self.parameters[i].kind, args[i], i)
            }
        }

        // The actual env is totally ignored right now
        let mut local_env = Environment::new(None);

        for i in 0..args.len() {
            local_env.declare_variable(&self.parameters[i].name, args[i])
        }

        self.body.evaluate(Some(&local_env))
    }
}



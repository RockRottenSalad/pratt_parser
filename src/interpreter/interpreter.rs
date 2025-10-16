#![allow(dead_code)]

use crate::utils::types::Either;
use crate::function::Function;
use crate::ast::{AstError, Expression, LiteralKind};
use crate::interpreter::statement::*;
use crate::parser::parser::{Parser, ParserError, parse_expression, parse_statement};
use crate::token::*;

use std::rc::Rc;

use std::collections::HashMap;
use std::fmt;
use std::path::Path;

#[derive(Debug)]
pub enum InterpreterError {
    NoError,

    Tokenizer(TokenizerError),
    Parser(ParserError),
    Ast(AstError),

    FileNotFound,
    CouldNotReadFile,

    ExpectedOneStatement,
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InterpreterError::NoError => write!(f, "No Error"),
            InterpreterError::Tokenizer(x) => write!(f, "{x}"),
            InterpreterError::Parser(x) => write!(f, "{x}"),
            InterpreterError::Ast(x) => write!(f, "{x}"),

            InterpreterError::FileNotFound => write!(f, "File not found"),
            InterpreterError::CouldNotReadFile => write!(f, "Could not read file"),
            InterpreterError::ExpectedOneStatement => write!(f, "Expected one statement - REPL"),
        }
    }
}

pub struct State {
    env: Box<Environment>
}

impl State {
    pub fn new() -> Self {
        State {
            env: Environment::new(None),
        }
    }

    pub fn env(&mut self) -> *mut Environment {
        &mut *self.env
    }

    pub fn borrow_env(&mut self) -> &mut Environment {
        &mut self.env
    }

    pub fn enter_scope(&mut self) {
        let prev = std::mem::replace(&mut self.env, Environment::new(None));
        self.env.set_parent(prev)
    }

    pub fn leave_scope(&mut self) -> Result<(), InterpreterError> {
        if !self.env.has_parent() {
            panic!("Interpreter performed illegal action: Left scope without parent")
        } else if self.env.has_weak_ref_to_parent() {
            panic!("Interpreter performed illegal action: Left scope with weak ref to parent")
        }

        self.env = self.env.parent();

        Ok(())
    }
}

/*
* The unsafe weak pointer BS is placeholder garbage until I find a better way to deal with
* functions. They introduce their own state combined with the current state. However, since that
* recursive call is made from within
*
*/
pub struct Environment {
    state: HashMap<Rc<str>, LiteralKind>,
    functions: HashMap<Rc<str>, Rc<Function>>,
    parent: Option<Either<Box<Environment>, *mut Environment>>,
}

impl Environment {
    pub fn new(parent: Option<Box<Environment>>) -> Box<Self> {
        Box::new(Environment {
            state: HashMap::with_capacity(10),
            functions: HashMap::with_capacity(10),
            parent: match parent {
                None => None,
                Some(p) => Some(Either::Left(p))
            },
        })
    }

    pub fn new_non_owning(parent: *mut Environment) -> Box<Self> {
        Box::new(Environment {
            state: HashMap::with_capacity(10),
            functions: HashMap::with_capacity(10),
            parent: Some(Either::Right(parent))
        })
    }

    pub fn set_parent(&mut self, parent: Box<Environment>) {
        self.parent = Some(Either::Left(parent));
    }

    pub fn parent(&mut self) -> Box<Self> {
        if self.parent.is_none() {
            panic!("Called '.parent()' on environment with no parent");
        }

        let tmp = std::mem::replace(&mut self.parent, None);

        match tmp.unwrap() {
            Either::Left(x) => x,
            _ => panic!("Unwrapped non-owning env - this should never occur")
        }
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn has_weak_ref_to_parent(&self) -> bool {
        match &self.parent {
            None => false,
            Some(p) => match p {
                Either::Left(_) => false,
                Either::Right(_) => true
            }
        }
    }

    pub fn declare_variable(&mut self, name: &str, value: LiteralKind) {
        self.state.insert(name.into(), value);
    }

    pub fn reassign_variable(&mut self, name: &str, value: LiteralKind) -> bool {
        if self.state.contains_key(name) {
            self.state.insert(name.into(), value);
            return true;
        }

        match &mut self.parent {
            None => { return false; },
            Some(env) => match env {
                Either::Left(x) => x.reassign_variable(name, value),
                Either::Right(x) => unsafe { (**x).reassign_variable(name, value) },
            }
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<LiteralKind> {
        match self.state.get(name) {
            Some(v) => Some(v.clone()),
            None => match &self.parent {
                Some(env) => match env {
                    Either::Left(x) => x.get_variable(name),
                    Either::Right(x) => unsafe { (**x).get_variable(name) },
                },
                None => None,
            },
        }
    }

    pub fn variable_exists(&self, name: &str) -> bool {
        return self.get_variable(name).is_some()
    }

    pub fn get_function(&self, name: &str) -> Option<&Function> {
        match self.functions.get(name) {
            Some(v) => Some(v),
            None => match &self.parent {
                Some(env) => match env {
                    Either::Left(x) => x.get_function(name),
                    Either::Right(x) => unsafe { (**x).get_function(name) },
                },
                None => None,
            },
        }
    }

    pub fn declare_function(&mut self, name: &str, f: Rc<Function>) {
        self.functions.insert(name.into(), f);
    }

    pub fn reassign_function(&mut self, name: &str, f: Rc<Function>) -> bool {
        if self.functions.contains_key(name) {
            self.functions.insert(name.into(), f);
            return true;
        }

        match &mut self.parent {
            None => { return false; },
            Some(env) => match env {
                Either::Left(x) => x.reassign_function(name, f),
                Either::Right(x) => unsafe { (**x).reassign_function(name, f) },
            }
        }
    }

}

pub fn interpret_as_exprs(
    input: &str,
) -> Result<Vec<Result<Box<Expression>, ParserError>>, InterpreterError> {
    let tokens = match tokenize(input) {
        Ok(v) => v,
        Err((e, _)) => return Err(InterpreterError::Tokenizer(e)),
    };

    let mut parser = Parser::new(&tokens);
    let mut exprs: Vec<Result<Box<Expression>, ParserError>> = Vec::with_capacity(10);

    while parser.peek() != Token::EOF {
        exprs.push(parse_expression(&mut parser));
    }

    Ok(exprs)
}

pub fn interpret_as_statements(
    input: &str,
) -> Result<Vec<Result<Box<Statement>, ParserError>>, InterpreterError> {
    let tokens = match tokenize(input) {
        Ok(v) => v,
        Err((e, _)) => return Err(InterpreterError::Tokenizer(e)),
    };

    let mut parser = Parser::new(&tokens);
    let mut stms: Vec<Result<Box<Statement>, ParserError>> = Vec::with_capacity(10);
    let is_in_repl_mode = false;

    while parser.peek() != Token::EOF {
        stms.push(parse_statement(&mut parser, is_in_repl_mode));
    }

    Ok(stms)
}

pub fn interpret_repl_mode(input: &str, state: &mut State) -> Result<(), InterpreterError> {
    let tokens = match tokenize(input) {
        Ok(v) => v,
        Err((e, _)) => return Err(InterpreterError::Tokenizer(e)),
    };

    let is_in_repl_mode = true;
    let mut parser = Parser::new(&tokens);
    let stm = parse_statement(&mut parser, is_in_repl_mode);

    match stm {
        Ok(v) => {
            if parser.peek() != Token::EOF {
                Err(InterpreterError::ExpectedOneStatement)
            } else {
                v.execute(state)
            }
        }
        Err(e) => Err(InterpreterError::Parser(e)),
    }
}

pub fn interpret_file(path: &Path) -> InterpreterError {
    if !path.exists() {
        return InterpreterError::FileNotFound;
    }

    let input = match std::fs::read_to_string(path) {
        Ok(v) => v,
        Err(_) => return InterpreterError::CouldNotReadFile,
    };

    let exprs = match interpret_as_statements(&input) {
        Ok(v) => v,
        Err(e) => return e,
    };

    let mut state = State::new();

    for expr in exprs {
        match expr {
            Ok(v) => match v.execute(&mut state) {
                Ok(()) => {}
                Err(e) => {
                    println!("Interpreter error: {}", e);
                }
            },
            Err(e) => println!("Parser error: {}", e),
        };
    }

    return InterpreterError::NoError;
}

#![allow(dead_code)] 

use crate::ast::{Expression, LiteralKind, AstError};
use crate::parser::parser::{parse, Parser, ParserError};
use crate::token::{*};
use crate::interpreter::statement::{*};

use std::fmt;
use std::path::Path;
use std::collections::HashMap;


#[derive(Debug)]
pub enum InterpreterError {
    NoError,

    Tokenizer(TokenizerError),
    Parser(ParserError),
    Ast(AstError),

    FileNotFound,
    CouldNotReadFile,

    ExpectedOneExpression
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
            InterpreterError::ExpectedOneExpression => write!(f, "Expected one expression - REPL"),
        }
    }
}


pub struct Environment<'a> {
    state: HashMap<&'a str, LiteralKind>,
    parent: Option<Box<Environment<'a>>>
}

impl<'a> Environment<'a> {
    pub fn new(parent: Option<Box<Environment<'a>>>) -> Box<Self> {
        Box::new(Environment { state: HashMap::with_capacity(10), parent })
    }

    pub fn parent(&mut self) -> Box<Self> {
        if self.parent.is_none() {
            panic!("Called '.parent()' on environment with no parent");
        }

        let tmp = std::mem::replace(&mut self.parent, None);
        tmp.unwrap()
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn declare_variable(&mut self, name: &'a str, value: LiteralKind) {
        self.state.insert(name, value);
    }

    pub fn get_variable(&mut self, name: &'a str) -> Option<&LiteralKind> {
        self.state.get(name)
    }

    pub fn variable_exists(&mut self, name: &'a str) -> bool {
        self.state.contains_key(name)
    }
}

pub fn supress_unused_statement_errors() -> Statement {
    todo!();
}

pub fn interpret_as_exprs(input: &str) -> Result<Vec<Result<Box<Expression>, ParserError>>, InterpreterError> {

    let tokens = match tokenize(input) {
        Ok(v) => v,
        Err((e,_)) => return Err( InterpreterError::Tokenizer(e) )
    };

    let mut parser = Parser::new(&tokens);
    let mut exprs: Vec<Result<Box<Expression>, ParserError>> = Vec::with_capacity(10);

    while parser.peek() != Token::EOF {
        exprs.push(parse(&mut parser));
    }

    Ok(exprs)
}

pub fn interpret(input: &str) -> Result<LiteralKind, InterpreterError> {
    let tokens = match tokenize(input) {
        Ok(v) => v,
        Err((e,_)) => return Err( InterpreterError::Tokenizer(e) )
    };

    let mut parser = Parser::new(&tokens);
    let expr = parse(&mut parser);

    if parser.peek() != Token::EOF {
        return Err(InterpreterError::ExpectedOneExpression);
    }


    match expr {
        Ok(v) => match v.evaluate() {
            Ok(r) => Ok(r),
            Err(e) => Err(InterpreterError::Ast(e)) 
        },
        Err(e) => Err(InterpreterError::Parser(e))
    }
}

pub fn interpret_file(path: &Path) -> InterpreterError {
    if !path.exists() {
        return InterpreterError::FileNotFound;
    }
    
    let input = match std::fs::read_to_string(path) {
        Ok(v) => v,
        Err(_) => return InterpreterError::CouldNotReadFile
    };

    let exprs = match interpret_as_exprs(&input) {
        Ok(v) => v,
        Err(_) => panic!("WIP")
    };

    for expr in exprs {
        match expr {
            Ok(v) => match v.evaluate() {
                Ok(r) => println!("{} = {}", v, r),
                Err(e) => println!("{} = {}", v, e),
            },
            Err(e) => println!("Parser error: {}", e)
        }
    }

    return InterpreterError::NoError;
}




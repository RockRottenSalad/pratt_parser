#![allow(dead_code)] 

use crate::ast::{Expression, LiteralKind, AstError};
use crate::parser::parser::{parse, Parser, ParserError};
use crate::token::{*};

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


struct Interpreter<'a> {
    environment: HashMap<&'a str, LiteralKind>,
}

impl<'a> Interpreter<'a> {
    fn new() -> Self {
        Interpreter { environment: HashMap::with_capacity(10) }
    }

    fn declare_variable(&mut self, name: &'a str, value: LiteralKind) {
        self.environment.insert(name, value);
    }

    fn get_variable(&mut self, name: &'a str) -> Option<&LiteralKind> {
        self.environment.get(name)
    }
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




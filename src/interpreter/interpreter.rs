#![allow(dead_code)] 

use std::fmt;
use std::path::Path;

use crate::ast::LiteralKind;
use crate::parser::parser::ParserError;
use crate::ast::AstError;

use crate::parser::parser::parse;
use crate::token::{*};


#[derive(Debug)]
pub enum InterpreterError {
    NoError,

    Tokenizer(TokenizerError),
    Parser(ParserError),
    Ast(AstError),

    FileNotFound,
    CouldNotReadFile,
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
        }
    }
}

pub fn interpret(input: &str) -> Result<LiteralKind, InterpreterError> {

    let tokens = match tokenize(input) {
        Ok(v) => v,
        Err((e,_)) => return Err( InterpreterError::Tokenizer(e) )
    };

    let ast = match parse(&tokens) {
        Ok(v) => v,
        Err(e) => return Err( InterpreterError::Parser(e) )
    };

    match ast.evaluate() {
        Ok(v) => Ok(v),
        Err(e) => Err( InterpreterError::Ast(e)  )
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

    for line in input.lines() {
        if line.len() == 0 {
            continue;
        }
        match interpret(line) {
            Ok(v) => println!("{line} = {v}"),
            Err(v) => println!("{line} = {v}")
        }
    }


    return InterpreterError::NoError;
}




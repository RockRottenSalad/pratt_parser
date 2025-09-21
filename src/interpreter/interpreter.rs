#![allow(dead_code)] 

use crate::ast::Expression;
use crate::parser::parser::Parser;
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

pub fn parse_expressions(input: &str) -> Result< Vec<Result<Box<Expression>, ParserError>>, InterpreterError > {

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
//    match ast.evaluate() {
//        Ok(v) => Ok(v),
//        Err(e) => Err( InterpreterError::Ast(e)  )
//    }
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

    let exprs = match parse_expressions(&input) {
        Ok(v) => v,
        Err(_) => panic!("WIP")
    };

    for expr in exprs {
        match expr {
            Ok(v) => match v.evaluate() {
                Ok(r) => println!("{} = {}", v, r),
                Err(e) => println!("{} = {}", v, e),
            },
            Err(e) => println!("{}", e)
        }
    }

//    for line in input.lines() {
//        if line.len() == 0 {
//            continue;
//        }
//        match interpret(line) {
//            Ok(v) => println!("{line} = {v}"),
//            Err(v) => println!("{line} = {v}")
//        }
//    }


    return InterpreterError::NoError;
}





mod parser;
mod tests;
mod utils;
mod interpreter;

use crate::interpreter::interpreter::{*};
//use crate::parser::parser::parse;
//use crate::token::{*};
use crate::parser::{*};

use std::env;
use std::io::BufRead;
use std::path::Path;

fn main() {

    let args = env::args();
    if args.len() < 2 {
        println!("Expected file path or \"cli\"");
        return;
    }

    let str = args.into_iter().nth(1).unwrap();

    if str == "cli" {
        let mut stdin = std::io::stdin().lock();
        loop {
            let mut buffer = String::new();
            let _ = stdin.read_line(&mut buffer);

            match interpret(&buffer) {
                Ok(v) => println!("{v}"),
                Err(e) => println!("{e}"),
            }
        }

    } else {
        let fp: &Path = Path::new(&str);
        interpret_file(fp);
    }


    // Should give 15
    //let input = "0.5 * 10/2 * (3 + 5) + (-5)";

    // Should give true
//    let input = "5 > 2 ? 5 : 2"; 
//
//    let tokens = match tokenize(input) {
//        Ok(v) => v,
//        Err((err, i)) => panic!("Tokenizer error: {err} at index {i}")
//    };
//
//    let ast = match parse(&tokens) {
//        Ok(v) => v,
//        Err(e) => panic!("Error parsing tokens:\n {:?}\n with error {:?}", tokens, e)
//    };
//
//    let output = match ast.evaluate() {
//        Ok(v) => v,
//        Err(e) => panic!("Syntax error: {:?}", e)
//    };
//
//    println!("{input}\n= {:?}\n= {ast}\n= {output}", tokens);
}


mod parser;
mod tests;
mod utils;

use crate::parser::parser::parse;
use crate::parser::{*};
use crate::token::{*};


fn main() {
    let input = "0.5 * 10/2 * (3 + 5) + (-5)";

    // Should give true
    //let input = "5 <= 25*5 / 2"; 

    let tokens = match tokenize(input) { // Should give 15
        Ok(v) => v,
        Err((err, i)) => panic!("Tokenizer error: {err} at index {i}")
    };

    let ast = match parse(&tokens) {
        Ok(v) => v,
        Err(e) => panic!("Error parsing tokens:\n {:?}\n with error {:?}", tokens, e)
    };

    let output = match ast.evaluate() {
        Ok(v) => v,
        Err(e) => panic!("Syntax error: {:?}", e)
    };

    println!("{input}\n= {:?}\n= {ast}\n= {output}", tokens);
}

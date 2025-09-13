
mod parser;
mod tests;
use crate::parser::parser::parse;
use crate::parser::{*};
use crate::token::{*};


fn main() {
    let input = "-(241 + 5) * 3 + -4/((+2))";

    let tokens = match tokenize(input) { // Should give -740
        Ok(v) => v,
        Err((err, i)) => panic!("Tokenizer error: {err} at index {i}")
    };

    let ast = match parse(&tokens) {
        Ok(v) => v,
        Err(e) => panic!("Error parsing tokens {:?} with error {:?}", tokens, e)
    };

    let output = match ast.evaluate() {
        Ok(v) => v,
        Err(e) => panic!("Syntax error: {:?}", e)
    };

    println!("{input}\n= {:?}\n= {ast}\n= {output}", tokens);
}

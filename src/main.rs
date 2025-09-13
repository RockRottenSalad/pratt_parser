
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

    let ast = parse(&tokens);

    let output = ast.evaluate();

    println!("{input}\n= {:?}\n= {ast}\n= {output}", tokens);
}

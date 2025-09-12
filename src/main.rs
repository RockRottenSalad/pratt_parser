
mod parser;
use crate::parser::parser::parse;
use crate::parser::{*};
use crate::tokenizer::{*};


fn main() {
    let mut tokenizer = match Tokenizer::new("5+9-4+2") {
        Ok(v) => v,
        Err((err, i)) => panic!("Tokenizer error: {err} at index {i}")
    };

    let ast = parse(&mut tokenizer);

    let output = ast.evaluate();

    println!("{tokenizer} = {ast} = {output}");
}

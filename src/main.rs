mod interpreter;
mod parser;
mod tests;
mod utils;

use crate::interpreter::interpreter::*;
//use crate::parser::parser::parse;
//use crate::token::{*};
use crate::parser::*;

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

}

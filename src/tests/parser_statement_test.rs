#[cfg(test)]
mod tests {
    use crate::token::*;
    use crate::interpreter::statement::{*};
    use crate::parser::parser::{*};
    use crate::ast::LiteralKind;

    #[test]
    fn test_parser_if_statements_good() {
        let inputs = [
            "if 5 > 10 { print 1 } else { print 2 }",
            "if 5*5 > 30 { print 1 } else if false { print 2 } else { print 3 }",
            "if 5*5 > 30 { print 1 } else if true { print 2 } else { print 3 }",
            "if 9 < 18 { print 1 } else { print 2 }",
            "if(10>=10){print 1}else{print 2}",
            "if true { print 2 + 3 * 2 } else { print 5 * 4 }",
        ];

        let expected = [ 2, 3, 2, 1, 1, 8, ];


        for (input, _output) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}"),
            };

            let mut parser = Parser::new(&tokens);

            let repl_mode = false;
            match parse_statement(&mut parser, repl_mode) {
                Ok(..) => {}, // TODO make interpreter tests that check this works
                Err(e) => panic!("Unexpected error for input {:?} | {e}", input),
            };
        }

    }

    #[test]
    fn test_var_dec_good() {

        let inputs = [
            "let x = 5",
            "let y = 25 * (2 - 10)",
            "let z = 10 > 5",
            "let longerName = 1.0",
        ];

        let expected = [
            ("x", LiteralKind::Integer(5)),
            ("y", LiteralKind::Integer(-200)),
            ("z", LiteralKind::Boolean(true)),
            ("longerName", LiteralKind::Real(1.0))
        ];

        for (input, (output_var, output_val)) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i} in {input}")
            };

            let mut parser = Parser::new(&tokens);

            let is_in_repl_mode = false;
            let stm = match parse_statement(&mut parser, is_in_repl_mode) {
                Ok(v) => v,
                Err(e) => panic!("Parser error: {e}")
            };

            match *stm {
                Statement::Assignment(str, expr) => 
                {
                    assert_eq!(*str, *output_var);
                    match expr.evaluate(None) {
                        Ok(v) => assert_eq!(v, output_val),
                        Err(e) => panic!("Failed to eval expr: {e}")
                    }
                }
                _ => panic!("Expected statement to be assignment")
            };

        }

    }

    #[test]
    fn test_var_dec_bad() {
        let inputs = [
            "lett extraLetter = 5",
            "et missingLetter = 25 * (2 - 10)",
            "let invalidExpr = 5 +",
            "let missingExpr =",
            "let missingEqual 5 * 2",
        ];

        for input in inputs {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}"),
            };

            let mut parser = Parser::new(&tokens);

            let repl_mode = false;
            match parse_statement(&mut parser, repl_mode) {
                Ok(s) => panic!("Expected parser to fail, got: {s}"),
                Err(..) => {} // TODO check that error makes sense
            }
        }
    }
}

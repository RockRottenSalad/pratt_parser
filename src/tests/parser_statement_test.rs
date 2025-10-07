#[cfg(test)]
mod tests {
    use crate::token::*;
    use crate::interpreter::statement::{*};
    use crate::parser::parser::{*};
    use crate::ast::LiteralKind;

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

        // TODO

        //let inputs = [
        //    "lett extraLetter = 5",
        //    "et missingLetter = 25 * (2 - 10)",
        //    "let invalidExpr = 5 +",
        //    "let missingExpr =",
        //];
    }
}

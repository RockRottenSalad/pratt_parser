// TODO:
// Add more bad tests

#[cfg(test)]
mod parser_e2e_test {

    use crate::ast::LiteralKind;
    use crate::parser::parser::Parser;
    use crate::parser::parser::ParserError;
    use crate::parser::parser::parse_expression;
    use crate::token::*;

    #[test]
    fn test_parser_e2e_integer_good() {
        let inputs = [
            "-(241 + 5) * 3 + -4/((+2))",
            "5*5*5*5",
            "--4",
            "-4",
            "20/4*2 + -4/(23*(-5*(23*(40))))",
            "6 / 2 * (1 + 2)",
            "-40*2 < -5*3 ? 1 : 0",
        ];

        let expected = [-740, 625, 4, -4, 10, 9, 1];

        for (input, output) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}"),
            };

            let mut parser = Parser::new(&tokens);

            let ast = match parse_expression(&mut parser) {
                Ok(v) => v,
                Err(e) => panic!("{e} in {:?}", tokens),
            };

            match ast.evaluate(None) {
                Ok(v) => assert_eq!(v, LiteralKind::Integer(output)),
                Err(e) => panic!("Syntax error: {:?}", e),
            }
        }
    }

    #[test]
    fn test_parser_e2e_real_good() {
        let inputs = [
            "-2.5 * 3 + 75.2",
            "0.5*0.5",
            "-1.0",
            "(20/4*2 + -4/(23*(-5*(23*(40))))) * 0.5",
            "5.5*3.7 >= 0.5 / 4.0 ? (42.5+0.5)*2 : 0.0+1",
        ];

        let expected = [67.7, 0.25, -1.0, 5.0, 86.0];

        for (input, output) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}"),
            };

            let mut parser = Parser::new(&tokens);

            let ast = match parse_expression(&mut parser) {
                Ok(v) => v,
                Err(e) => panic!("{e} in {:?}", tokens)
            };

            match ast.evaluate(None) {
                Ok(v) => match v {
                    LiteralKind::Real(x) => assert!((x - output).abs() < 0.01),
                    _ => panic!("Expected a real"),
                },
                Err(e) => panic!("Syntax error: {:?}", e),
            }
        }
    }

    #[test]
    fn test_parser_e2e_boolean_good() {
        let inputs = ["5 > 10", "25*2 > 10 / 5 * 3"];

        let expected = [false, true];

        for (input, output) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}"),
            };

            let mut parser = Parser::new(&tokens);

            let ast = match parse_expression(&mut parser) {
                Ok(v) => v,
                Err(e) => panic!("{e} in {:?}", tokens),
            };

            match ast.evaluate(None) {
                Ok(v) => match v {
                    LiteralKind::Boolean(x) => assert!(x == output),
                    _ => panic!("Expected a boolean"),
                },
                Err(e) => panic!("Syntax error: {:?}", e),
            }
        }
    }

    #[test]
    fn test_parser_if_statements_good() {
        let inputs = [
            "if 5 > 10 { 1 } else { 2 }",
            "if 5*5 > 30 { 1 } else if false { 2 } else { 3 }",
            "if 5*5 > 30 { 1 } else if true { 2 } else { 3 }",
            "if 9 < 18 { 1 } else { 2 }",
            "if(10>=10){1}else{2}",
            "if true { 2 + 3 * 2 } else { 5 * 4 }",
        ];

        let expected = [ 2, 3, 2, 1, 1, 8, ];


        for (input, output) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}"),
            };

            let mut parser = Parser::new(&tokens);

            match parse_expression(&mut parser) {
                Ok(v) => match v.evaluate(None).unwrap() {
                    LiteralKind::Integer(x) => assert_eq!(x, output),
                    _ => panic!("Unexpected type")
                },
                Err(e) => panic!("Unexpected error {e}"),
            };
        }

    }

    #[test]
    fn test_parser_e2e_bad() {
        let inputs = ["5*", "(5+10", "??"];

        let expected = [
            ParserError::ExpectedLiteral(2),
            ParserError::UnterminatedGrouping(4),
            ParserError::ExpectedLiteral(1),
        ];

        for (input, output) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}"),
            };

            let mut parser = Parser::new(&tokens);

            match parse_expression(&mut parser) {
                Ok(v) => panic!("Should failed, instead got value {v}"),
                Err(e) => assert_eq!(e, output),
            };
        }
    }
}


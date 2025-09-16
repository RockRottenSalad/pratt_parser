
// TODO:
// Add bad tests

#[cfg(test)]
mod parser_e2e_test {

    use crate::ast::LiteralKind;
    use crate::token::{*};
    use crate::parse;
    use crate::parser::parser::ParserError;

    #[test]
    fn test_parser_e2e_integer_good() {

        let inputs = [
            "-(241 + 5) * 3 + -4/((+2))",
            "5*5*5*5",
            "--4",
            "-4",
            "20/4*2 + -4/(23*(-5*(23*(40))))"
        ];

        let expected = [
            -740,
            625,
            4,
            -4,
            10
        ];

        for (input, output) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}")
            };

            let ast = match parse(&tokens) {
                Ok(v) => v,
                Err(e) => match e {
                    ParserError::UnterminatedGrouping(index) => panic!("Unterminated grouping at index {index} in {:?}", tokens),
                    ParserError::ExpectedOperator(index) => panic!("Expected operator at index {index} in {:?}", tokens),
                    ParserError::ExpectedLiteral(index) => panic!("Expected literal at index {index} in {:?}", tokens),
                    ParserError::SyntaxError(index) => panic!("Syntax error at index {index} in {:?}", tokens),
                }
            };

            match ast.evaluate() {
                Ok(v) => assert_eq!(v, LiteralKind::Integer(output)),
                Err(e) => panic!("Syntax error: {:?}", e)
            }
        }

    }

    #[test]
    fn test_parser_e2e_real_good() {

        let inputs = [
            "-2.5 * 3 + 75.2",
            "0.5*0.5",
            "-1.0",
            "(20/4*2 + -4/(23*(-5*(23*(40))))) * 0.5"
        ];

        let expected = [
            67.7,
            0.25,
            -1.0,
            5.0
        ];

        for (input, output) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}")
            };

            let ast = match parse(&tokens) {
                Ok(v) => v,
                Err(e) => match e {
                    ParserError::UnterminatedGrouping(index) => panic!("Unterminated grouping at index {index} in {:?}", tokens),
                    ParserError::ExpectedOperator(index) => panic!("Expected operator at index {index} in {:?}", tokens),
                    ParserError::ExpectedLiteral(index) => panic!("Expected literal at index {index} in {:?}", tokens),
                    ParserError::SyntaxError(index) => panic!("Syntax error at index {index} in {:?}", tokens),
                }
            };

            match ast.evaluate() {
                Ok(v) => match v {
                    LiteralKind::Real(x) => assert!( (x - output).abs() < 0.01 ),
                    _ => panic!("Expected a real"),
                }
                Err(e) => panic!("Syntax error: {:?}", e)
            }
        }

    }

    #[test]
    fn test_parser_e2e_boolean_good() {

        let inputs = [
            "5 > 10",
            "25*2 > 10 / 5 * 3"
        ];

        let expected = [
            false,
            true
        ];

        for (input, output) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}")
            };

            let ast = match parse(&tokens) {
                Ok(v) => v,
                Err(e) => match e {
                    ParserError::UnterminatedGrouping(index) => panic!("Unterminated grouping at index {index} in {:?}", tokens),
                    ParserError::ExpectedOperator(index) => panic!("Expected operator at index {index} in {:?}", tokens),
                    ParserError::ExpectedLiteral(index) => panic!("Expected literal at index {index} in {:?}", tokens),
                    ParserError::SyntaxError(index) => panic!("Syntax error at index {index} in {:?}", tokens),
                }
            };

            match ast.evaluate() {
                Ok(v) => match v {
                    LiteralKind::Boolean(x) => assert!(x==output),
                    _ => panic!("Expected a boolean"),
                }
                Err(e) => panic!("Syntax error: {:?}", e)
            }
        }

    }

    #[test]
    fn test_parser_e2e_bad() {

        let inputs = [
            "5*",
            "4 5",
            "(5+10",
        ];
        

        let expected = [
            ParserError::ExpectedLiteral(2),
            ParserError::ExpectedOperator(1),
            ParserError::UnterminatedGrouping(4),
        ];

        for (input, output) in std::iter::zip(inputs, expected).into_iter() {
            let tokens = match tokenize(input) {
                Ok(v) => v,
                Err((e, i)) => panic!("Tokenizer error: {e} at index {i}")
            };

            // NOTE: This will return a result in the future
            match parse(&tokens) {
                Ok(v) => panic!("Should failed, instead got value {v}"),
                Err(e) => assert_eq!(e, output)
            };

        }
    }
}

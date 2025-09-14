
// TODO:
// Add bad tests

#[cfg(test)]
mod parser_e2e_test {

    use crate::token::{*};
    use crate::parse;
    use crate::parser::parser::ParserError;

    #[test]
    fn test_parser_e2e_good() {

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

            // NOTE: This will return a result in the future
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
                Ok(v) => assert_eq!(v, output),
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

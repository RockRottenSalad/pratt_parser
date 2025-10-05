#[cfg(test)]
mod ast_tests {
    use crate::ast::*;

    #[test]
    fn test_ast_evaluate_integer_good() {
        let expected = -6;

        // -(20+5*2) / 2 + 9
        let ast = Expression::BinaryAddition(
            Box::new(Expression::BinaryDivision(
                Box::new(Expression::UnaryNegation(Box::new(Expression::Grouping(
                    Box::new(Expression::BinaryAddition(
                        Box::new(Expression::Literal(LiteralKind::Integer(20))),
                        Box::new(Expression::BinaryMultiplication(
                            Box::new(Expression::Literal(LiteralKind::Integer(5))),
                            Box::new(Expression::Literal(LiteralKind::Integer(2))),
                        )),
                    )),
                )))),
                Box::new(Expression::Literal(LiteralKind::Integer(2))),
            )),
            Box::new(Expression::Literal(LiteralKind::Integer(9))),
        );

        match ast.evaluate(None) {
            Ok(actual) => assert_eq!(actual, LiteralKind::Integer(expected)),
            Err(e) => panic!("Failed to evaluate AST, got error: {:?}", e),
        }
    }

    #[test]
    fn test_ast_evaluate_real_good() {
        let expected = 0.46;

        // 2.3 * 2 / 10
        let ast = Expression::BinaryDivision(
            Box::new(Expression::BinaryMultiplication(
                Box::new(Expression::Literal(LiteralKind::Real(2.3))),
                Box::new(Expression::Literal(LiteralKind::Integer(2))),
            )),
            Box::new(Expression::Literal(LiteralKind::Integer(10))),
        );

        match ast.evaluate(None) {
            Ok(actual) => match actual {
                LiteralKind::Real(x) => assert!((x - expected).abs() < 0.01),
                _ => panic!("Expected real"),
            },
            Err(e) => panic!("Failed to evaluate AST, got error: {:?}", e),
        }
    }

    #[test]
    fn test_ast_evaluate_boolean_good() {
        // 25 * 3 >= 4 / 2 * 3
        let ast = Expression::GreaterEqualThan(
            Box::new(Expression::BinaryMultiplication(
                Box::new(Expression::Literal(LiteralKind::Integer(25))),
                Box::new(Expression::Literal(LiteralKind::Integer(3))),
            )),
            Box::new(Expression::BinaryMultiplication(
                Box::new(Expression::BinaryDivision(
                    Box::new(Expression::Literal(LiteralKind::Integer(4))),
                    Box::new(Expression::Literal(LiteralKind::Integer(2))),
                )),
                Box::new(Expression::Literal(LiteralKind::Integer(3))),
            )),
        );

        match ast.evaluate(None) {
            Ok(actual) => match actual {
                LiteralKind::Boolean(x) => assert!(x),
                _ => panic!("Expected boolean"),
            },
            Err(e) => panic!("Failed to evaluate AST, got error: {:?}", e),
        }
    }

    #[test]
    fn test_ast_evaluate_bad() {
        let ast = Expression::BinaryMultiplication(
            Box::new(Expression::Literal(LiteralKind::Integer(5))),
            Box::new(Expression::BinaryDivision(
                Box::new(Expression::Literal(LiteralKind::Integer(5))),
                Box::new(Expression::Literal(LiteralKind::Integer(0))),
            )),
        );

        match ast.evaluate(None) {
            Ok(v) => panic!("Should have failed due to divsion by zero, got value {}", v),
            Err(e) => match e {
                AstError::DivisionByZero => {}
                _ => panic!("Expected division by zero error, got {:?}", e),
            },
        }

        // Parser no longer allows for this
        //        let ast = Expression::Unary(Operator::Division, Box::new(Expression::Literal(LiteralKind::Integer(10))));
        //
        //        match ast.evaluate(None) {
        //            Ok(v) => panic!("Should have failed due to illegal unary operator error, got value {}", v),
        //            Err(e) => match e {
        //                AstError::IllegalUnaryOperator => {},
        //                _ => panic!("Expected illegal unary operator error, got {:?}", e)
        //            }
        //        }
    }
}

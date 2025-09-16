
#[cfg(test)]
mod ast_tests {
    use crate::ast::{*};

    #[test]
    fn test_ast_evaluate_good() {

        let expected = -6;

        // -(20+5*2) / 2 + 9
        let ast = Expression::Binary(Operator::Addition, 
            Box::new(Expression::Binary(Operator::Division,
                Box::new(Expression::Unary(Operator::Subtraction,
                    Box::new(Expression::Grouping(Box::new(Expression::Binary(Operator::Addition,
                        Box::new(Expression::Literal(LiteralKind::Integer(20))),
                        Box::new(
                            Expression::Binary(Operator::Multiplication, 
                                Box::new(Expression::Literal(LiteralKind::Integer(5))),
                                Box::new(Expression::Literal(LiteralKind::Integer(2))))
                        ))))))),
                Box::new(Expression::Literal(LiteralKind::Integer(2))))),
            Box::new(Expression::Literal(LiteralKind::Integer(9)))
        );

        match ast.evaluate() {
            Ok(actual) => assert_eq!(actual, LiteralKind::Integer(expected)),
            Err(e) => panic!("Failed to evaluate AST, got error: {:?}", e)
        }
    }

    #[test]
    fn test_ast_evaluate_bad() {

        let ast = Expression::Binary(Operator::Multiplication,
            Box::new(Expression::Literal(LiteralKind::Integer(5))),
            Box::new(Expression::Binary(Operator::Division,
                Box::new(Expression::Literal(LiteralKind::Integer(5))),
                Box::new(Expression::Literal(LiteralKind::Integer(0)))))
        );

        match ast.evaluate() {
            Ok(v) => panic!("Should have failed due to divsion by zero, got value {}", v),
            Err(e) => match e {
                AstError::DivisionByZero => {},
                _ => panic!("Expected division by zero error, got {:?}", e)
            }
        }

        let ast = Expression::Unary(Operator::Division, Box::new(Expression::Literal(LiteralKind::Integer(10))));

        match ast.evaluate() {
            Ok(v) => panic!("Should have failed due to illegal unary operator error, got value {}", v),
            Err(e) => match e {
                AstError::IllegalUnaryOperator => {},
                _ => panic!("Expected illegal unary operator error, got {:?}", e)
            }
        }
    }

}

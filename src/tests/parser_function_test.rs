#[cfg(test)]
mod tests {
    use crate::State;
    use crate::ast::Expression;
    use crate::token::*;
    use crate::interpreter::statement::{*};
    use crate::parser::parser::{*};
    use crate::ast::LiteralKind;

    #[test]
    fn test_parser_single_arg_function_good() {
        let input = "let doubleIT = fn (x int) -> 2*x";

        let func_inputs = [1, 2, 3, 4];
        let expected: Vec<i32> = func_inputs.iter().map(|&x| 2*x).collect();

        let tokens = match tokenize(input) {
            Ok(v) => v,
            Err((e, i)) => panic!("Tokenizer error: {e} at index {i}"),
        };

        let mut parser = Parser::new(&tokens);
        
        let repl_mode = false;
        let stm = match parse_statement(&mut parser, repl_mode) {
            Ok(s) => s,
            Err(e) => panic!("Unexpected error for input {:?} | {e}", input),
        };


        let mut state = State::new();

        match *stm {
            Statement::FunctionAssignment(name, func) => 
            state.borrow_env_mut().declare_function(&name, func),
            _ => panic!("Expected, function assignment statement")
        }

        for (x, y) in std::iter::zip(func_inputs, expected).into_iter() {
            let literal = LiteralKind::Integer(x);
            let expected = LiteralKind::Integer(y);

            let expr = Expression::FunctionCall("doubleIT".into(), vec![Box::new(Expression::Literal(literal))]);

            let res = expr.evaluate(Some(&mut state));

            match res {
                Ok(v) => assert_eq!(v, expected),
                Err(e) => panic!("Function call failed: {e}")
            }

        }

    }

    #[test]
    fn test_parser_body_function_good() {
        let input = "let func = fn (x int) -> {  let y = x * 2 return x * y  }";

        let func_inputs = [1, 2, 3, 4];
        let expected: Vec<i32> = func_inputs.iter().map(|&x| 2*x*x).collect();

        let tokens = match tokenize(input) {
            Ok(v) => v,
            Err((e, i)) => panic!("Tokenizer error: {e} at index {i}"),
        };

        let mut parser = Parser::new(&tokens);
        
        let repl_mode = false;
        let stm = match parse_statement(&mut parser, repl_mode) {
            Ok(s) => s,
            Err(e) => panic!("Unexpected error for input {:?} | {e}", input),
        };


        let mut state = State::new();

        match *stm {
            Statement::FunctionAssignment(name, func) => 
            state.borrow_env_mut().declare_function(&name, func),
            _ => panic!("Expected, function assignment statement")
        }

        for (x, y) in std::iter::zip(func_inputs, expected).into_iter() {
            let literal = LiteralKind::Integer(x);
            let expected = LiteralKind::Integer(y);

            let expr = Expression::FunctionCall("func".into(), vec![Box::new(Expression::Literal(literal))]);

            let res = expr.evaluate(Some(&mut state));

            match res {
                Ok(v) => assert_eq!(v, expected),
                Err(e) => panic!("Function call failed: {e}")
            }

        }
    }
}

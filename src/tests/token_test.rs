#[cfg(test)]
mod tests {
    use crate::token::*;

    #[test]
    fn test_char_to_token_good() {
        let input = ['+', '-', '*', '/', '(', ')', ' ', '\t', '\n'];
        let expected = [
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::ParenL,
            Token::ParenR,
            Token::Space,
            Token::Space,
            Token::Space,
        ];

        for (i, ch) in input.iter().map(|ch| char_to_token(*ch)).enumerate() {
            assert!(
                ch.is_ok(),
                "Checking that mapping the car to a token succeeded"
            );
            assert_eq!(
                ch.unwrap(),
                expected[i],
                "Checking that the token equals the expected token"
            );
        }
    }

    #[test]
    fn test_char_to_token_bad() {
        // [1] char_to_token() is not used for tokenizing literals
        //                                          v [1]
        let input = ['|', '_', '^', '#', 'a', 'B', '4'];

        for ch in input.iter().map(|ch| char_to_token(*ch)) {
            assert!(
                ch.is_err(),
                "Checking that mapping the char to a token failed"
            );
        }
    }

    #[test]
    fn test_tokenize_good() {
        // NOTE: Current intended behaviour is that tokenize() filters out spaces
        let input = "-(241 + 5) * 3 + -4/((+2))";

        let expected = [
            Token::Minus,
            Token::ParenL,
            Token::LiteralInteger(241),
            Token::Plus,
            Token::LiteralInteger(5),
            Token::ParenR,
            Token::Star,
            Token::LiteralInteger(3),
            Token::Plus,
            Token::Minus,
            Token::LiteralInteger(4),
            Token::Slash,
            Token::ParenL,
            Token::ParenL,
            Token::Plus,
            Token::LiteralInteger(2),
            Token::ParenR,
            Token::ParenR,
        ];

        let actual = match tokenize(input) {
            Ok(v) => v,
            Err((e, i)) => panic!("Tokenizer failed to tokenize with error: {e} at index {i}"),
        };

        assert_eq!(
            expected.len(),
            actual.len(),
            "Checking that expected length and actual length are equal"
        );
        for (i, tok) in actual.iter().enumerate() {
            assert_eq!(*tok, expected[i]);
        }
    }

    #[test]
    fn test_tokenize_bad() {
        let input = "20/5 * 10 + % - 2*(-4)";
        let expected_index_error = 12;
        let expected_char_error = '%';

        match tokenize(input) {
            Ok(_) => panic!("Tokenize should've failed"),
            Err((TokenizerError::IllegalToken(actual_ch), actual_index)) => {
                assert_eq!(expected_index_error, actual_index);
                assert_eq!(expected_char_error, actual_ch);
            }
            _ => panic!("Should've been an illegal token error"), // Implement this when more error types are added
                                                                  //            Err(_) => panic!("Error type should've been IllegalToken")
        }
    }
}

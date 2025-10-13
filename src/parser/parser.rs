#![allow(dead_code)]

// TODO
// Should tolerate invalid tokens i.e. Space tokens if they make their way through the tokenizer
// More fault tolerance + should return Result type

use crate::function::{Function,Argument};
use crate::interpreter::statement::Statement;
use crate::{
    parser::ast::{Expression, LiteralKind},
    token::Token,
};
use std::fmt;

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser { tokens, index: 0 }
    }

    pub fn consume(&mut self) {
        if self.tokens_remaining() > 0 {
            self.index += 1;
        }
    }

    pub fn next(&mut self) -> Token {
        if self.index == self.tokens.len() {
            return Token::EOF;
        }

        let ret = &self.tokens[self.index];
        self.index += 1;

        ret.clone()
    }

    pub fn peek(&self) -> Token {
        if self.index == self.tokens.len() {
            return Token::EOF;
        }

        self.tokens[self.index].clone()
    }

    pub fn tokens_remaining(&self) -> usize {
        self.tokens.len() - self.index
    }

    pub fn reset(&mut self) {
        self.index = 0;
    }

    pub fn rewind(&mut self, to: usize) {
        self.index = to
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    UnterminatedGrouping(usize),
    ExpectedOperator(usize),
    ExpectedLiteral(usize),
    ExpectedType(usize),
    ExpectedIdentifier(usize),
    SyntaxError(usize),
    ExpectedToken(usize, Token),

    ExpectedStatement(usize),
}

// TODO: Since there is a tokens array, that can be used alongside the index to give a better error
// message. The index of a token doesn't map well to a string. Especially when you factor in the
// fact that '>=' is counted as two tokens
impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::UnterminatedGrouping(x) => write!(f, "Unterminated grouping at token {x}"),
            ParserError::ExpectedOperator(x) => write!(f, "Expected operator at token {x}"),
            ParserError::ExpectedLiteral(x) => write!(f, "Expected literal at token {x}"),
            ParserError::ExpectedType(x) => write!(f, "Expected type at token {x}"),
            ParserError::ExpectedIdentifier(x) => write!(f, "Expected identifier at token {x}"),
            ParserError::SyntaxError(x) => write!(f, "Syntax error at token {x}"),
            ParserError::ExpectedToken(x, t) => write!(f, "Expected token {t} at token {x}"),

            ParserError::ExpectedStatement(x) => write!(f, "Expected statement at token {x}"),
        }
    }
}

fn consume_until_token_or_eof(token: &Token, p: &mut Parser) -> () {
    while p.peek() != *token && p.peek() != Token::EOF {
        p.consume();
    }
}

fn expect(token: Token, p: &mut Parser) -> Result<Token, ParserError> {
    if p.peek() != token {
        let index = p.index();
        consume_until_token_or_eof(&token, p);
        Err(ParserError::ExpectedToken(index, token))
    } else {
        Ok(p.next())
    }
}

fn grouping_expr_handler(p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    let expr = parse_expr(p, 0)?;

    match expect(Token::ParenR, p) {
        Err(e) => match e {
            ParserError::ExpectedToken(i, _) => return Err(ParserError::UnterminatedGrouping(i)),
            _ => panic!("Expect should currently only retturn expected token errors"),
        },
        Ok(_) => {}
    };

    Ok(Box::new(Expression::Grouping(expr)))
}

fn typecast_expr_handler(expr: Box<Expression>, p: &mut Parser) -> Result<Box<Expression>, ParserError> {

    let cast_type = match p.peek() {
        Token::Int => LiteralKind::default_integer(),
        Token::Real => LiteralKind::default_real(),
        Token::Bool => LiteralKind::default_boolean(),
        _ => return Err(ParserError::ExpectedType(p.index()))
    };

    p.consume();
    Ok(Box::new(Expression::Typecast(expr, cast_type)))
}

fn args_expr_handler(p: &mut Parser) -> Result<Vec<LiteralKind>, ParserError> {
    let mut v = Vec::new();

    expect(Token::ParenL, p)?;

    match p.next() {
        Token::LiteralInteger(x) => v.push(LiteralKind::Integer(x)),
        _ => v.push(LiteralKind::Integer(0))
    }

    expect(Token::ParenR, p)?;

    Ok(v)
}

fn head_handler(token: Token, p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    match token {
        // IF statement
        //        Token::If => if_statement_handler(p),
        //        Token::CurlyL => block_statement_handler(p),
        Token::LiteralInteger(x) => Ok(Box::new(Expression::Literal(LiteralKind::Integer(x)))),
        Token::LiteralReal(x) => Ok(Box::new(Expression::Literal(LiteralKind::Real(x)))),
        Token::LiteralBoolean(x) => Ok(Box::new(Expression::Literal(LiteralKind::Boolean(x)))),

        Token::Identifier(x) => match p.peek() {
            Token::ParenL => Ok(Box::new(Expression::FunctionCall(x, args_expr_handler(p)?))),
            _ => Ok(Box::new(Expression::Reference(x))),
        },

        Token::Minus => Ok(Box::new(Expression::UnaryNegation(parse_expr(
            p,
            token.precedence(),
        )?))),
        Token::Plus => Ok(Box::new(Expression::UnaryAddition(parse_expr(
            p,
            token.precedence(),
        )?))),
        Token::ParenL => grouping_expr_handler(p),
        _ => Err(ParserError::ExpectedLiteral(p.index())),
    }
}

fn tail_handler(
    token: Token,
    expr: Box<Expression>,
    p: &mut Parser,
) -> Result<Box<Expression>, ParserError> {
    match token {
        Token::LiteralInteger(_) => Err(ParserError::ExpectedOperator(p.index())),
        Token::LiteralReal(_) => Err(ParserError::ExpectedOperator(p.index())),
        Token::Plus => Ok(Box::new(Expression::BinaryAddition(
            expr,
            parse_expr(p, token.precedence())?,
        ))),
        Token::Minus => Ok(Box::new(Expression::BinarySubtraction(
            expr,
            parse_expr(p, token.precedence())?,
        ))),
        Token::Star => Ok(Box::new(Expression::BinaryMultiplication(
            expr,
            parse_expr(p, token.precedence())?,
        ))),
        Token::Slash => Ok(Box::new(Expression::BinaryDivision(
            expr,
            parse_expr(p, token.precedence())?,
        ))),

        Token::As => typecast_expr_handler(expr, p),

        Token::And => Ok(Box::new(Expression::And(expr, parse_expr(p, 0)?))),
        Token::Or => Ok(Box::new(Expression::Or(expr, parse_expr(p, 0)?))),

        Token::Question => {
            let left = parse_expr(p, 0)?;

            if p.peek() != Token::Colon {
                return Err(ParserError::SyntaxError(p.index()));
            } else {
                p.consume();
            }

            // TODO implement expected an expression error and use that here
            if p.peek() == Token::EOF {
                return Err(ParserError::ExpectedLiteral(p.index()));
            }

            let right = parse_expr(p, 0)?;

            Ok(Box::new(Expression::Ternary(expr, left, right)))
        }
        Token::GreaterThan => {
            if p.peek() == Token::Equal {
                p.consume();
                Ok(Box::new(Expression::GreaterEqualThan(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            } else {
                Ok(Box::new(Expression::GreaterThan(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            }
        }
        Token::LessThan => {
            if p.peek() == Token::Equal {
                p.consume();
                Ok(Box::new(Expression::LessEqualThan(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            } else {
                Ok(Box::new(Expression::LessThan(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            }
        }
        Token::Equal => {
            if p.peek() == Token::Equal {
                p.consume();
                Ok(Box::new(Expression::EqualTo(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            } else {
                Err(ParserError::ExpectedToken(p.index() - 1, Token::Let))
            }
        }
        Token::Bang => {
            if p.peek() == Token::Equal {
                p.consume();
                Ok(Box::new(Expression::NotEqualTo(
                    expr,
                    parse_expr(p, token.precedence())?,
                )))
            } else {
                Err(ParserError::SyntaxError(p.index()))
            }
        }
        _ => Err(ParserError::SyntaxError(p.index())),
    }
}

//fn ternary_handler(predicate: Box<Expression>, p: &mut Parser) 
//-> Result<Box<Expression>, ParserError> {
//    expect(Token::Question, p)?;
//
//    let left = parse_expression(p)?;
//    expect(Token::Colon, p)?;
//    let right = parse_expression(p)?;
//
//    Ok(Box::new(Expression::Ternary(predicate, left, right)))
//}

fn parse_expr(p: &mut Parser, expr_precedence: u8) -> Result<Box<Expression>, ParserError> {
    let mut current = p.next();
    let mut left_expr = head_handler(current, p)?;

    while p.peek().precedence() > expr_precedence {
        current = p.next();
        left_expr = tail_handler(current, left_expr, p)?;
    }

    Ok(left_expr)
}

fn function_handler(p: &mut Parser) -> Result<Function, ParserError> {

    expect(Token::Fn, p)?;

    expect(Token::ParenL, p)?;

    p.consume();
//    expect(Token::Identifier("".into()), p)?;
    expect(Token::Int, p)?;

    expect(Token::ParenR, p)?;

    expect(Token::Minus, p)?;
    expect(Token::GreaterThan, p)?;

    let expr = parse_expr(p, 0)?;
    let arg = Argument {
        name: "x".into(),
        kind: LiteralKind::Integer(0)
    };

    let func = Function {parameters: vec![arg],
        body: expr, return_value: LiteralKind::Integer(0)
    };

    return Ok(func)
}

fn let_statement_handler(p: &mut Parser) -> Result<Box<Statement>, ParserError> {
    expect(Token::Let, p)?;

    let id = match p.next() {
        Token::Identifier(str) => str,
        _ => return Err(ParserError::ExpectedIdentifier(p.index() - 1)),
    };

    expect(Token::Equal, p)?;

    match p.peek() {
        Token::Fn => Ok(Box::new(Statement::FunctionAssignment(id, function_handler(p)?.into()))),
        _ => Ok(Box::new(Statement::Assignment(id, parse_expr(p, 0)?)))
    }

}

fn print_statement_handler(p: &mut Parser, expect_print: bool) -> Result<Box<Statement>, ParserError> {
    if expect_print {
        expect(Token::Print, p)?;
    }

    Ok(Box::new(Statement::Print(parse_expr(p, 0)?)))
}

fn block_statement_handler(p: &mut Parser, repl_mode: bool) -> Result<Box<Statement>, ParserError> {
    expect(Token::CurlyL, p)?;

    let mut stms = Vec::with_capacity(10);
    while p.peek() != Token::CurlyR && p.peek() != Token::EOF {
        stms.push(parse_statement(p, repl_mode)?)
    }

    expect(Token::CurlyR, p)?;

    stms.shrink_to_fit();
    Ok(Box::new(Statement::Block(stms)))
}

fn if_statement_handler(p: &mut Parser, repl_mode: bool) -> Result<Box<Statement>, ParserError> {
    expect(Token::If, p)?;

    let condition = parse_expr(p, 0)?;

    let statement = match p.peek() {
        Token::CurlyL => block_statement_handler(p, repl_mode)?,
        _             => parse_statement(p, repl_mode)?,
    };

    if p.peek() == Token::Else {
        p.consume();

        Ok(Box::new(Statement::IfElse(
            condition,
            statement,
            parse_statement(p, repl_mode)?,
        )))
    } else {
        Ok(Box::new(Statement::If(condition, statement)))
    }
}

pub fn parse_expression(p: &mut Parser) -> Result<Box<Expression>, ParserError> {
    parse_expr(p, 0)
}

pub fn parse_statement(p: &mut Parser, repl_mode: bool) -> Result<Box<Statement>, ParserError> {
    // When in REPL mode, specifying 'print' token not required
    let should_expect_print_token = !repl_mode;

    let stm = match p.peek() {
        Token::Let => let_statement_handler(p),
        Token::Print => print_statement_handler(p, true),
        Token::If => if_statement_handler(p, repl_mode),
        Token::CurlyL => block_statement_handler(p, repl_mode),
        // For maintaing backwards compat -- should Err in future
        _ => {
            if repl_mode {
                print_statement_handler(p, should_expect_print_token)
            } else {
                let index = p.index();
                // Consume expected expr to prevent further errors
                let _ = parse_expr(p, 0);
                Err(ParserError::ExpectedStatement(index))
            }
        }
    };

    // If this statement failed, then we want to consume the rest of the statement, so that this
    // error doesn't cause any further errors.
    //    if stm.is_err() {
    //        while p.peek() != Token::EOF && let Err(..) = parse_statement(p, repl_mode) {}
    //    }

    stm
}

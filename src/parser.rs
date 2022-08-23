use crate::lexer::{Lexer, TokenKind, Keyword, Token, Operator};

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Expression {
    String(String),
    Identifier(String),
    Integer(i64),
    Operator(Operator, Box<Expression>, Box<Expression>),
    Print(Box<Expression>),
    GotoIf(String, Box<Expression>),
    Set(String, Box<Expression>)
    // Hello(Box<Expression>, Box<Expression>)
}

#[derive(Debug, Copy, Clone)]
pub enum ParserError {
    UnexpectedToken {
        expected: TokenKind, found: Option<TokenKind>, span: (usize, usize)
    },

    UnterminatedString {
        span: (usize, usize)
    },

    OperatorOverflow {
        expected: usize,
        found: usize,
        span: (usize, usize)
    },

    NonLValueAssign
}

fn expect_identifier(token: Option<Token>) -> Result<String, ParserError> {
    if let Some(token) = token {
        if token.kind() != TokenKind::Identifier {
            Err(ParserError::UnexpectedToken { expected: TokenKind::Identifier, found: Some(token.kind()), span: token.span() })
        } else {
            Ok(token.data().to_owned())
        }
    } else {
        // TODO: fix span 
        Err(ParserError::UnexpectedToken { expected: TokenKind::Identifier, found: None, span: (0,0) })
    }
}

fn parse_string(token: Token) -> Result<Expression, ParserError> {
    let mut terminated = false;
    let mut string = String::new();

    let mut chars = token.data()
        .chars().skip(1);

    while let Some(c) = chars.next() {
        match c {
            '"' => {
                terminated = true; 
                break;
            },

            '\\' => string.push(match chars.next() {
                Some('"') => '"',
                Some('n') => '\n',
                Some(c) => c,

                None => return Err(ParserError::UnterminatedString { span: token.span() })
            }),

            c => string.push(c)
        }
    }

    if terminated {
        Ok(Expression::String(string))
    } else {
        Err(ParserError::UnterminatedString { span: token.span() })
    }
    
    
}

pub fn parse(lexer: Lexer) -> Result<(Vec<Expression>, HashMap<String, usize>), ParserError> {
    let mut stack = Vec::new();
    let mut labels = HashMap::new();

    let mut lexer = lexer.filter(|t| !matches!(t.kind(), TokenKind::Whitespace | TokenKind::Comment));

    while let Some(token) = lexer.next() {
        let expr = match token.kind() {
            TokenKind::Integer => Expression::Integer(token.data().parse().unwrap()),
            TokenKind::Identifier => Expression::Identifier(token.data().to_owned()),
            TokenKind::StringLit => parse_string(token)?,

            TokenKind::Keyword(Keyword::Label) => {
                let name = expect_identifier(lexer.next())?;
                labels.insert(name, stack.len());
                continue;
            }

            TokenKind::Keyword(Keyword::GotoIf) => {
                let val = stack.pop()
                    .ok_or(ParserError::OperatorOverflow { expected: 1, found: 0, span: token.span() })?;
                let name = expect_identifier(lexer.next())?;

                Expression::GotoIf(name, Box::new(val))
            }

            TokenKind::Keyword(Keyword::Set) => {
                let val = stack.pop()
                    .ok_or(ParserError::OperatorOverflow { expected: 1, found: 0, span: token.span() })?;

                let name = stack.pop()
                    .ok_or(ParserError::OperatorOverflow { expected: 1, found: 0, span: token.span() })?;
                
                if let Expression::Identifier(name) = name {
                    Expression::Set(name, Box::new(val))
                } else {
                    return Err(ParserError::NonLValueAssign)
                }
            }

            TokenKind::Keyword(Keyword::Print) => {
                let val = stack.pop()
                    .ok_or(ParserError::OperatorOverflow { expected: 1, found: 0, span: token.span() })?;

                Expression::Print(Box::new(val))
            }

            TokenKind::Operator(op) => match (stack.pop(), stack.pop()) {
                (None, None) => return Err(ParserError::OperatorOverflow { expected: 2, found: 0, span: token.span() }),
                (None, Some(_)) => return Err(ParserError::OperatorOverflow { expected: 2, found: 1, span: token.span() }),
                (Some(n2), Some(n1)) => Expression::Operator(op, Box::new(n1), Box::new(n2)),

                pair => unimplemented!("{:?}", pair)
            }

            _ => unimplemented!("{:?}", token)
        };

        stack.push(expr);
    } 

    Ok((stack, labels))
} 
use crate::lexer::{Lexer, TokenKind, Keyword, Token, Operator};

#[derive(Debug, Clone)]
pub enum Expression {
    String(String),
    Integer(i64),
    Operator(Operator, Box<Expression>, Box<Expression>),
    Print(Box<Expression>),
    // Hello(Box<Expression>, Box<Expression>)
}

#[derive(Debug, Copy, Clone)]
pub enum ParserError {
    // UnexpectedToken {
    //     expected: TokenKind, found: Option<TokenKind>, span: (usize, usize)
    // },

    UnterminatedString {
        span: (usize, usize)
    },

    OperatorOverflow {
        expected: usize,
        found: usize,
        span: (usize, usize)
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

pub fn parse(lexer: Lexer) -> Result<Vec<Expression>, ParserError> {
    let mut stack = Vec::new();
    let mut lexer = lexer.filter(|t| !matches!(t.kind(), TokenKind::Whitespace | TokenKind::Comment));

    while let Some(token) = lexer.next() {
        let expr = match token.kind() {
            TokenKind::Integer => Expression::Integer(token.data().parse().unwrap()),
            TokenKind::StringLit => parse_string(token)?,

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

    Ok(stack)
} 
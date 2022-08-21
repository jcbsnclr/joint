use crate::lexer::{Lexer, TokenKind, Keyword, Token};

#[derive(Debug, Copy, Clone)]
pub enum Expression {
    Print(i64)
}

#[derive(Debug, Copy, Clone)]
pub enum ParserError {
    UnexpectedToken {
        expected: TokenKind, found: Option<TokenKind>, span: (usize, usize)
    }
}

fn next_token<'a>(lexer: &'a mut Lexer) -> Option<Token<'a>> {
    lexer.filter(|t| t.kind() != TokenKind::Whitespace).next()
}

fn expect_token<'a>(lexer: &'a mut Lexer, kind: TokenKind) -> Result<Token<'a>, ParserError> {
    let pos = lexer.pos();

    let next = next_token(lexer)
        .ok_or(ParserError::UnexpectedToken { expected: kind, found: None, span: (pos, pos + 1) })?;

    if next.kind() == kind {
        Ok(next)
    } else {
        Err(ParserError::UnexpectedToken { expected: kind, found: Some(next.kind()), span: next.span() })
    }
}

pub fn parse(mut lexer: Lexer) -> Result<Vec<Expression>, ParserError> {
    let mut exprs = Vec::new();

    while let Some(next) = next_token(&mut lexer) {
        let expr = match next.kind() {
            TokenKind::Keyword(Keyword::Print) => {
                let val: i64 = expect_token(&mut lexer, TokenKind::Integer)
                    .unwrap()
                    .data()
                    .parse()
                    .unwrap();
    
                Expression::Print(val)
            },
            
            k => unimplemented!("{:?}", k)
        };

        exprs.push(expr)
    }

    Ok(exprs)
} 
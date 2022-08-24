use std::ops::{Deref, DerefMut};
use std::iter::Peekable;

use crate::lexer::{Lexer, TokenKind, Keyword, Token, BinaryOperator, UnaryOperator};

#[derive(Debug, Clone)]
pub enum ExprData {
    Integer(i64),
    Add(Box<Expr>, Box<Expr>),
    Print(Box<Expr>)
}

#[derive(Debug, Clone)]
pub struct Expr {
    span: (usize, usize),
    data: ExprData
}

#[derive(Debug, Clone)]
pub enum DeclarationKind {
    Func {
        name: String,
        args: Vec<String>,
        body: Vec<Expr>
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    span: (usize, usize),
    kind: DeclarationKind
}

#[derive(Debug, Clone)]
pub struct CompilationUnit {
    span: (usize, usize),
    decls: Vec<Declaration>
}

#[deprecated]
#[derive(Debug, Clone)]
pub enum Expression {
    String(String),
    Identifier(String),
    Integer(i64),
    BinaryOperator(BinaryOperator, Box<Expression>, Box<Expression>),
    UnaryOperator(UnaryOperator, Box<Expression>),
    Print(Box<Expression>),
    Label(String),
    GotoIf(String, Box<Expression>),
    Decl(String),
    Set(String, Box<Expression>),
    Call(String),
    DoLoopIf(Box<Expression>, Vec<Expression>),
    Func(String, Vec<String>, Vec<Expression>),
    Return,
    Ip,
    // Hello(Box<Expression>, Box<Expression>)
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

                None => return Err(ParserError {
                    span: Some(token.span()),
                    kind: ParserErrorKind::UnterminatedStringLiteral
                })
            }),

            c => string.push(c)
        }
    }

    if terminated {
        Ok(Expression::String(string))
    } else {
        Err(ParserError {
            span: Some(token.span()),
            kind: ParserErrorKind::UnterminatedStringLiteral
        })
    }
}

struct ExprStack(Vec<Vec<Expr>>);

impl ExprStack {
    pub fn new() -> ExprStack {
        ExprStack(vec![Vec::new()])
    }
    
    pub fn push(&mut self, expr: Expr) {
        self.0
            .iter_mut()
            .last()
            .unwrap()
            .push(expr);
    }

    pub fn pop(&mut self) -> Option<Expr> {
        self.0
            .iter_mut()
            .last()
            .unwrap()
            .pop()
    }

    pub fn push_stack(&mut self) {
        self.0.push(Vec::new());
    }

    pub fn pop_stack(&mut self) -> Option<Vec<Expr>> {
        self.0.pop()
    }

    pub fn last_mut(&mut self) -> Option<&mut Expr> {
        self.0
            .iter_mut()
            .last()
            .unwrap()
            .iter_mut()
            .last()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ParserErrorKind<'a> {
    UnexpectedToken {
        expected: &'a [TokenKind],
        found: Option<TokenKind>
    },

    ExpectedToken {
        expected: TokenKind,
        found: Option<TokenKind>
    },

    BinaryOperatorUnderflow {
        found: usize
    },

    FunctionUnderflow {
        expected: usize,
        found: usize
    },
    
    UnterminatedStringLiteral,
    UnexpectedEof,
}

#[derive(Debug, Copy, Clone)]
pub struct ParserError<'a> {
    span: Option<(usize, usize)>,
    kind: ParserErrorKind<'a>
}

fn expect_next<'a>(lexer: &mut Lexer<'a>, pred: &'a [TokenKind]) -> Result<Token<'a>, ParserError<'a>> {
    let next = lexer.next_token()
        .ok_or(ParserError {
            span: None,
            kind: ParserErrorKind::UnexpectedToken { expected: pred, found: None }
        })?;

    if !pred.contains(&next.kind()) {
        Err(ParserError {
            span: Some(next.span()),
            kind: ParserErrorKind::UnexpectedToken { expected: pred, found: Some(next.kind()) }
        })
    } else {
        Ok(next)
    }
}

fn expect_identifier<'a>(lexer: &mut Lexer<'a>) -> Result<String, ParserError<'a>> {
    Ok(expect_next(lexer, &[TokenKind::Identifier])?.data().to_owned())
}

fn extract_identifier<'a>(lexer: &mut Lexer<'a>, token: Option<Token<'a>>) -> Result<String, ParserError<'a>> {
    Ok(token.ok_or(ParserError {
        span: None,
        kind: ParserErrorKind::UnexpectedToken { expected: &[TokenKind::Identifier], found: None }
    })?.data().to_owned())
}

fn expect_keyword<'a>(lexer: &mut Lexer<'a>, kw: Keyword) -> Result<Token<'a>, ParserError<'a>> {
    let next = lexer.next_token()
        .ok_or(ParserError {
            span: None,
            kind: ParserErrorKind::ExpectedToken { expected: TokenKind::Keyword(kw), found: None }
        })?;

    match next.kind() {
        TokenKind::Keyword(k) if kw == k => Ok(next),
        _ => Err(ParserError {
            span: Some(next.span()),
            kind: ParserErrorKind::ExpectedToken { expected: TokenKind::Keyword(kw), found: Some(next.kind()) }
        })
    }
}

fn parse_expr<'a>(lexer: &mut Lexer<'a>, stack: &mut ExprStack) -> Result<(), ParserError<'a>> {
    let next = lexer.next_token()
        .ok_or(ParserError {
            span: None,
            kind: ParserErrorKind::UnexpectedEof
        })?;

    let (mut start, mut end) = next.span();

    let expr = match next.kind() {
        TokenKind::Integer => Ok(Expr {
            span: next.span(),
            data: ExprData::Integer(next.data().parse().expect("lexer misidentifier integer"))
        }),

        TokenKind::BinaryOperator(BinaryOperator::Plus) => {
            let n2 = stack.pop()
                .ok_or(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::BinaryOperatorUnderflow { found: 0 }
                })?;
            let n1 = stack.pop()
                .ok_or(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::BinaryOperatorUnderflow { found: 1 }
                })?;

            start = n1.span.0;

            Ok(Expr {
                span: (start, end),
                data: ExprData::Add(Box::new(n1), Box::new(n2))
            })
        },

        TokenKind::Keyword(Keyword::Print) => {
            let val = stack.pop()
                .ok_or(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::FunctionUnderflow { expected: 1, found: 0 }
                })?;

            Ok(Expr {
                span: (start, end),
                data: ExprData::Print(Box::new(val))
            })
        }

        _ => unimplemented!("{:?}", next)
    }?;

    stack.push(expr);
    Ok(())
}

pub fn parse<'a>(lexer: &mut Lexer<'a>) -> Result<CompilationUnit, ParserError<'a>> {
    let mut decls = Vec::new();

    let (start, mut end) = (0, 1);

    while let Some(next) = lexer.next_token() {
        (_, end) = next.span();

        match next.kind() {
            TokenKind::Keyword(Keyword::Func) => {
                let mut args = Vec::new();
                
                loop {
                    let next = expect_next(lexer, &[
                        TokenKind::Identifier, TokenKind::Keyword(Keyword::Does)
                    ])?;

                    if next.kind() == TokenKind::Keyword(Keyword::Does) {
                        end = next.span().1; 
                        break;
                    } else {
                        let ident = next.data()
                            .to_owned();

                        args.push(ident);
                    }
                }

                let name = args.pop().unwrap();

                let mut stack = ExprStack::new();

                while let Some(t) = lexer.peek_token() {
                    println!("{:?}", t.kind());
                    if t.kind() == TokenKind::Keyword(Keyword::Done) {
                        break;
                    }
                    parse_expr(lexer, &mut stack)?;
                }

                let t = expect_keyword(lexer, Keyword::Done)?;

                end = t.span().1;

                decls.push(Declaration { span: (start, end), kind: DeclarationKind::Func { name, args, body: stack.pop_stack().unwrap() } })
            },

            _ => unimplemented!("{:?}", next)
        }
    }

    Ok(CompilationUnit {
        span: (start, end),
        decls
    })
}
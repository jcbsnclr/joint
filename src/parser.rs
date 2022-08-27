use crate::lexer::{Lexer, TokenKind, Keyword, Token, BinOp, UnOp};

#[derive(Debug, Clone)]
pub enum ExprData {
    Integer(i64),

    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),

    // VarDecl(String),
    Set(String, Box<Expr>),
    Ident(String),

    Print(Box<Expr>),

    DoLoopIf(Box<Expr>, Vec<Expr>),

    Var(String, i64),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: (usize, usize),
    pub data: ExprData
}

#[derive(Debug, Clone)]
pub enum DeclarationKind {
    Func {
        name: String,
        args: Vec<String>,
        body: Vec<Expr>
    },
    Var {
        name: String,
        value: i64
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub span: (usize, usize),
    pub kind: DeclarationKind
}

#[derive(Debug, Clone)]
pub struct CompilationUnit {
    pub span: (usize, usize),
    pub decls: Vec<Declaration>
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

    ExpectedIdentifier,
    ExpectedExpression,

    BinaryOperatorUnderflow {
        found: usize
    },
    UnaryOperatorOverflow,

    FunctionUnderflow {
        expected: usize,
        found: usize
    },
    
    UnterminatedStringLiteral,
    UnexpectedEof,
}

#[derive(Debug, Copy, Clone)]
pub struct ParserError<'a> {
    pub span: Option<(usize, usize)>,
    pub kind: ParserErrorKind<'a>
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

fn expect_identifier<'a>(lexer: &mut Lexer<'a>) -> Result<Token<'a>, ParserError<'a>> {
    Ok(expect_next(lexer, &[TokenKind::Identifier])?)
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

        TokenKind::Keyword(Keyword::Var) => {
            let name = expect_identifier(lexer)?;
            let val = expect_next(lexer, &[TokenKind::Integer])?;

            Ok(Expr {
                span: (next.span().0, val.span().1),
                data: ExprData::Var(name.data().to_owned(), val.data().parse().unwrap())
            })
        }

        TokenKind::BinOp(op) => {
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
                data: ExprData::BinOp(op, Box::new(n1), Box::new(n2))
            })
        },

        TokenKind::UnOp(op) => {
            let n = stack.pop()
                .ok_or(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::UnaryOperatorOverflow,
                })?;

            start = n.span.0;

            Ok(Expr {
                span: (start, end),
                data: ExprData::UnOp(op, Box::new(n))
            })
        }

        TokenKind::Keyword(Keyword::Set) => {
            let val = stack.pop()
                .ok_or(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::UnexpectedEof
                })?;

            let (name, name_span) = stack.pop()
                .ok_or(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::UnexpectedEof
                })
                .map(|v| (v.clone(), v.span))?;

            if let Expr { data: ExprData::Ident(name), ..} = name {
                Ok(Expr {
                    span: (name_span.0, next.span().1),
                    data: ExprData::Set(name, Box::new(val))
                })
            } else {
                Err(ParserError {
                    span: Some((name_span.0, next.span().1)),
                    kind: ParserErrorKind::ExpectedIdentifier
                })
            }
        }

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

        TokenKind::Keyword(Keyword::Do) => {
            stack.push_stack();
            parse_expr(lexer, stack)?;
            return Ok(());
        }

        TokenKind::Keyword(Keyword::LoopIf) => {
            let mut body = stack.pop_stack().unwrap();

            let cond = body.pop()
                .ok_or(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::ExpectedExpression
                })?;

            Ok(Expr {
                span: (0, 0),
                data: ExprData::DoLoopIf(Box::new(cond), body)
            })
        }

        TokenKind::Identifier => {
            let name = next.data().to_owned();

            Ok(Expr {
                span: (start, end),
                data: ExprData::Ident(name),
            })
        },

        // TokenKind::Keyword(Keyword::Var) => {
        //     let name = expect_identifier(lexer)?;

        //     Ok(Expr {
        //         span: (name.span().0, next.span().1),
        //         data: ExprData::VarDecl(name.data().to_owned())
        //     })
        // }

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

                if args.len() != 0 {
                    panic!("function arguments currently unsupported");
                }

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

            TokenKind::Keyword(Keyword::Var) => {
                let name = expect_identifier(lexer)?;
                let val = expect_next(lexer, &[TokenKind::Integer])?;

                decls.push(Declaration { span: (next.span().0, val.span().1), kind: DeclarationKind::Var {
                    name: name.data().to_owned(), value: val.data().parse().unwrap()
                }});
            }

            _ => unimplemented!("{:?}", next)
        }
    }

    Ok(CompilationUnit {
        span: (start, end),
        decls
    })
}
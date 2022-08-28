use crate::lexer::{Lexer, TokenKind, Keyword, Token, BinOp, UnOp};
use crate::visitors::validator::{Type, IntrinsicType};

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum ExprData {
    Integer(i64),
    StringLit(String),

    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),

    // VarDecl(String),
    Set(String, Box<Expr>),
    Get(String),

    FnCall(String, Vec<Expr>),

    Print(Box<Expr>),

    DoLoopIf(Box<Expr>, Vec<Expr>),
    DoWhile(Box<Expr>, Vec<Expr>),
    DoBlock(Vec<Expr>),

    TypeCast(Box<Expr>, Type),

    PrintType(Box<Expr>),

    Var(String, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: (usize, usize),
    pub data: ExprData,
    pub typ: Option<Type>,
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
        value: Box<Expr>
    },
    Type {
        var: String,
        typ: Type
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

    fn size(&self) -> usize {
        self.0.len()
    }

    // pub fn last_mut(&mut self) -> Option<&mut Expr> {
    //     self.0
    //         .iter_mut()
    //         .last()
    //         .unwrap()
    //         .iter_mut()
    //         .last()
    // }
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

    UnknownType(&'a str),
    
    UnterminatedStringLiteral,
    UnexpectedEof,
}

#[derive(Debug, Copy, Clone)]
pub struct ParserError<'a> {
    pub span: Option<(usize, usize)>,
    pub kind: ParserErrorKind<'a>
}

#[derive(Debug, Clone)]
pub struct TypeExpr {
    pub span: (usize, usize),
    pub data: Type
}

fn parse_type<'a>(lexer: &mut Lexer<'a>) -> Result<Type, ParserError<'a>> {
    let mut type_stack = Vec::new();

    while let Some(t) = lexer.next_token() {
        if t.kind() == TokenKind::Keyword(Keyword::Done) {
            break;
        } else if t.kind() != TokenKind::Identifier {
            return Err(ParserError {
                span: Some(t.span()),
                kind: ParserErrorKind::ExpectedIdentifier
            })
        }

        let expr = match t.data() {
            "U8" => Ok(Type::Concrete(IntrinsicType::U8)),
            "I8" => Ok(Type::Concrete(IntrinsicType::I8)),
    
            "U16" => Ok(Type::Concrete(IntrinsicType::U16)),
            "I16" => Ok(Type::Concrete(IntrinsicType::I16)),
    
            "U32" => Ok(Type::Concrete(IntrinsicType::U32)),
            "I32" => Ok(Type::Concrete(IntrinsicType::I32)),
    
            "U64" => Ok(Type::Concrete(IntrinsicType::U64)),
            "I64" => Ok(Type::Concrete(IntrinsicType::I64)),
    
            "String" => Ok(Type::Concrete(IntrinsicType::String)),

            "Func" => {
                let args = type_stack.drain(..)
                    .collect();

                Ok(Type::Function { args })
            }
    
            _ => Err(ParserError {
                span: Some(t.span()),
                kind: ParserErrorKind::UnknownType(t.data())
            })
        }?;

        type_stack.push(expr);
    }

    Ok(type_stack.pop().unwrap())
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

fn parse_expr<'a>(lexer: &mut Lexer<'a>, stack: &mut ExprStack, fnargs: &mut HashMap<String, usize>) -> Result<(), ParserError<'a>> {
    let next = lexer.next_token()
        .ok_or(ParserError {
            span: None,
            kind: ParserErrorKind::UnexpectedEof
        })?;

    let (mut start, mut end) = next.span();

    let expr = match next.kind() {
        TokenKind::Integer => Ok(Expr {
            span: next.span(),
            data: ExprData::Integer(next.data().parse().expect("lexer misidentifier integer")),
            typ: None
        }),

        TokenKind::Keyword(Keyword::Var) => {
            let name = expect_identifier(lexer)?;
            parse_expr(lexer, stack, fnargs)?;
            let val = stack.pop().unwrap();

            Ok(Expr {
                span: (next.span().0, val.span.1),
                data: ExprData::Var(name.data().to_owned(), Box::new(val)),
                typ: None
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
                data: ExprData::BinOp(op, Box::new(n1), Box::new(n2)),
                typ: None
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
                data: ExprData::UnOp(op, Box::new(n)),
                typ: None
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

            if let Expr { data: ExprData::Get(name), ..} = name {
                Ok(Expr {
                    span: (name_span.0, next.span().1),
                    data: ExprData::Set(name, Box::new(val)),
                    typ: None
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
                data: ExprData::Print(Box::new(val)),
                typ: None
            })
        }

        TokenKind::Keyword(Keyword::PrintType) => {
            let expr = stack.pop()
                .ok_or(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::FunctionUnderflow { expected: 1, found: 0 }
                })?;

            Ok(Expr {
                span: (start, end),
                data: ExprData::PrintType(Box::new(expr)),
                typ: None
            })
        }

        TokenKind::Keyword(Keyword::As) => {
            let expr = stack.pop()
                .ok_or(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::FunctionUnderflow { expected: 1, found: 0 }
                })?;

            let typ = parse_type(lexer)?;

            Ok(Expr {
                span: (start, end),
                data: ExprData::TypeCast(Box::new(expr), typ.clone()),
                typ: None
            })
        }

        TokenKind::Keyword(Keyword::Do) => {
            stack.push_stack();
            parse_expr(lexer, stack, fnargs)?;
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
                data: ExprData::DoLoopIf(Box::new(cond), body),
                typ: None
            })
        }

        TokenKind::Keyword(Keyword::While) => {
            let mut body = stack.pop_stack().unwrap();

            let cond = body.pop()
                .ok_or(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::ExpectedExpression
                })?;

            Ok(Expr {
                span: (0, 0),
                data: ExprData::DoWhile(Box::new(cond), body),
                typ: None
            })
        }

        TokenKind::Keyword(Keyword::Done) => {
            let body = stack.pop_stack().unwrap();

            Ok(Expr {
                span: (0, 0),
                data: ExprData::DoBlock(body),
                typ: None
            })
        }

        TokenKind::Identifier => {
            let name = next.data().to_owned();
            
            if let Some(n) = fnargs.get(&name) {
                let mut args = Vec::new();

                for _ in 0..*n {
                    args.push(stack.pop().unwrap());
                }

                Ok(Expr {
                    span: (start, end),
                    data: ExprData::FnCall(name, args),
                    typ: None
                })
            } else {
                Ok(Expr {
                    span: (start, end),
                    data: ExprData::Get(name),
                    typ: None
                })
            }
        },

        TokenKind::StringLit => {
            if next.data().chars().skip(1).last() != Some('"') {
                Err(ParserError {
                    span: Some(next.span()),
                    kind: ParserErrorKind::UnterminatedStringLiteral
                })
            } else {
                let len = next.data().len();
                let body = &next.data()[1..len-1];

                Ok(Expr {
                    span: next.span(),
                    data: ExprData::StringLit(body.to_owned()),
                    typ: None
                })
            }
        }

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

    let mut fnargs = HashMap::new();

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

                fnargs.insert(name.clone(), args.len());

                let mut stack = ExprStack::new();

                while let Some(t) = lexer.peek_token() {
                    dbg!(t.kind());
                    if t.kind() == TokenKind::Keyword(Keyword::Done) && stack.size() == 1 {
                        break;
                    }
                    parse_expr(lexer, &mut stack, &mut fnargs)?;
                }

                let t = expect_keyword(lexer, Keyword::Done)?;

                end = t.span().1;

                decls.push(Declaration { span: (start, end), kind: DeclarationKind::Func { name, args, body: stack.pop_stack().unwrap() } })
            },

            TokenKind::Keyword(Keyword::Var) => {
                let name = expect_identifier(lexer)?;
                let mut stack = ExprStack::new();
                parse_expr(lexer, &mut stack, &mut fnargs)?;
                let val = stack.pop().unwrap();

                decls.push(Declaration { span: (next.span().0, val.span.1), kind: DeclarationKind::Var {
                    name: name.data().to_owned(), value: Box::new(val)
                }});
            }

            TokenKind::Keyword(Keyword::Type) => {
                let name = expect_identifier(lexer)?;
                let typ = parse_type(lexer)?;

                decls.push(Declaration { span: next.span(), kind: DeclarationKind::Type {
                    var: name.data().to_owned(), typ: typ
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
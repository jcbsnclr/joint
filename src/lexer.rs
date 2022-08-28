use std::str::CharIndices;
use std::iter::Peekable;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    Print,
    Set,
    Var,
    Call,
    Return,
    Do,
    LoopIf,
    While,
    Func,
    Does,
    Done,
    Type,
    PrintType,
    As,

    Ip
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Hyphen,
    Asterisk,
    Slash,
    Equals,

    And,
    Or
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
    Not
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Keyword(Keyword),
    BinOp(BinOp),
    UnOp(UnOp),
    StringLit,
    Identifier,
    Integer,
    Whitespace,
    Comment
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    data: &'a str,
    span: (usize, usize),
    kind: TokenKind
}

impl<'a> Token<'a> {
    pub fn data(&self) -> &'a str        { self.data }
    pub fn span(&self) -> (usize, usize) { self.span }
    pub fn kind(&self) -> TokenKind      { self.kind }
}

/// a lexer turns a source file into a series of machine readable tokens
pub struct Lexer<'a> {
    source: &'a str,
    stream: Peekable<CharIndices<'a>>,
    pos: usize,
    peek: Option<Token<'a>>
}

impl<'a> Lexer<'a> {
    /// anitialize lexer with a string
    pub fn from_source(source: &'a str) -> Lexer<'a> {
        let mut lx = Lexer { source: source, stream: source.char_indices().peekable(), pos: 0, peek: None };
        lx.peek = lx.advance();
        lx
    }

    pub fn pos(&self) -> usize {
        self.pos
    }
}

fn identifier_start(c: char) -> bool {
    !c.is_whitespace() && !c.is_numeric() && !c.is_control()
}

fn identifier_body(c: char) -> bool {
    !c.is_whitespace() && !c.is_control()
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        let (_,c) = self.stream.peek()?;

        match c {
            '#' => self.comment(),
            c if c.is_whitespace() => self.whitespace(),

            c if identifier_start(*c) => self.identifier(),
            '0' ..= '9' => self.integer(),
            '"'         => self.string_lit(),

            c => unimplemented!("Unhandled case: '{}'", c)
        }
    }
}

impl<'a> Lexer<'a> {
    fn whitespace(&mut self) -> Option<Token<'a>> {
        let (start,_) = self.stream.next()?;
        let mut end = start + 1;

        while let Some((pos,_)) = self.stream.next_if(|(_,c)| c.is_whitespace()) {
            end = pos + 1;
        }

        let data = &self.source[start..end];

        self.pos = end;

        Some(Token {
            data,
            span: (start, end),
            kind: TokenKind::Whitespace
        })
    }

    fn comment(&mut self) -> Option<Token<'a>> {
        let (start,_) = self.stream.next()?;
        let mut end = start + 1;

        while let Some((pos,_)) = self.stream.next_if(|&(_,c)| c != '\n') {
            end = pos + 1;
        }

        let data = &self.source[start..end];

        self.pos = end;

        Some(Token {
            data,
            span: (start, end),
            kind: TokenKind::Comment
        })
    }

    fn identifier(&mut self) -> Option<Token<'a>> {
        let (start, _) = self.stream.next()?;
        let mut end = start + 1;

        while let Some((pos,_)) = self.stream.next_if(|(_,c)| identifier_body(*c)) {
            end = pos + 1;
        }

        let data = &self.source[start..end];
        
        let kind = match data {
            "+" => TokenKind::BinOp(BinOp::Plus),
            "-" => TokenKind::BinOp(BinOp::Hyphen),
            "*" => TokenKind::BinOp(BinOp::Asterisk),
            "/" => TokenKind::BinOp(BinOp::Slash),
            "=" => TokenKind::BinOp(BinOp::Equals),

            "print" => TokenKind::Keyword(Keyword::Print),
            "print-type" => TokenKind::Keyword(Keyword::PrintType),
            "call" => TokenKind::Keyword(Keyword::Call),
            "return" => TokenKind::Keyword(Keyword::Return),
            "set" => TokenKind::Keyword(Keyword::Set),
            "var" => TokenKind::Keyword(Keyword::Var),
            "do" => TokenKind::Keyword(Keyword::Do),
            "?loop" => TokenKind::Keyword(Keyword::LoopIf),
            "?while" => TokenKind::Keyword(Keyword::While),

            "func" => TokenKind::Keyword(Keyword::Func),
            "does" => TokenKind::Keyword(Keyword::Does),
            "done" => TokenKind::Keyword(Keyword::Done),

            "type" => TokenKind::Keyword(Keyword::Type),
            "as" => TokenKind::Keyword(Keyword::As),

            "$ip" => TokenKind::Keyword(Keyword::Ip),

            "and" => TokenKind::BinOp(BinOp::And),
            "or" => TokenKind::BinOp(BinOp::Or),
            
            "not" => TokenKind::UnOp(UnOp::Not),

            _ => TokenKind::Identifier
        };

        self.pos = end;

        Some(Token {
            data,
            span: (start, end),
            kind
        })
    }

    fn integer(&mut self) -> Option<Token<'a>> {
        let (start, _) = self.stream.next()?;
        let mut end = start + 1;

        while let Some((pos,_)) = self.stream.next_if(|(_,c)| matches!(c, '0' ..= '9')) {
            end = pos + 1;
        }

        let data = &self.source[start..end];

        self.pos = end;

        Some(Token {
            data,
            span: (start, end),
            kind: TokenKind::Integer
        })
    }

    fn string_lit(&mut self) -> Option<Token<'a>> {
        let (start,_) = self.stream.next()?;
        let mut end = start + 1;

        while let Some((pos,_)) = self.stream.next_if(|&(_,c)| c != '"') {
            end = pos + 1;
        }

        let data = &self.source[start..end];

        self.pos = end;

        Some(Token {
            data,
            span: (start, end),
            kind: TokenKind::StringLit
        })
    }
}

impl<'a> Lexer<'a> {
    fn advance(&mut self) -> Option<Token<'a>> {
        let next = self.next()?;

        if matches!(next.kind(), TokenKind::Whitespace | TokenKind::Comment) {
            self.advance()
        } else {
            Some(next)
        }
    }

    pub fn peek_token(&mut self) -> Option<Token<'a>> {
        self.peek
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        let t = self.peek_token();
        self.peek = self.advance();
        t
    }
}
use std::str::CharIndices;
use std::iter::Peekable;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    Hello
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Hyphen,
    Asterisk,
    Slash
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Keyword(Keyword),
    Operator(Operator),
    StringLit,
    Identifier,
    Integer,
    Whitespace
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
    pos: usize
}

impl<'a> Lexer<'a> {
    /// anitialize lexer with a string
    pub fn from_source(source: &'a str) -> Lexer<'a> {
        Lexer { source: source, stream: source.char_indices().peekable(), pos: 0 }
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
            c if identifier_start(*c) => self.identifier(),
            '0' ..= '9' => self.integer(),
            '"'         => self.string_lit(),

            c if c.is_whitespace() => self.whitespace(),

            c => unimplemented!("Unhandled case: '{}'", c)
        }
    }
}

impl<'a> Lexer<'a> {
    fn identifier(&mut self) -> Option<Token<'a>> {
        let (start, _) = self.stream.next()?;
        let mut end = start + 1;

        while let Some((pos,_)) = self.stream.next_if(|(_,c)| identifier_body(*c)) {
            end = pos + 1;
        }

        let data = &self.source[start..end];
        
        let kind = match data {
            "+" => TokenKind::Operator(Operator::Plus),
            "-" => TokenKind::Operator(Operator::Hyphen),
            "*" => TokenKind::Operator(Operator::Asterisk),
            "/" => TokenKind::Operator(Operator::Slash),

            "hello" => TokenKind::Keyword(Keyword::Hello),

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

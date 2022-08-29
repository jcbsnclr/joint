//! The lexer is the first stage in the process of running some Turbo code. 
//! 
//! When the user provides a source file to the interpreter, it will load the contents of the 
//! source file into a string and create a [Lexer] with a view into the source string, which is 
//! then passed to the parser (see: [crate::visitors::parser::parse]). The [Lexer] produces a 
//! stream of [Token]s - sections of the string, categorised based on their contents - which the 
//! parser then uses to produce a tree providing context to the source file.

use std::str::CharIndices;
use std::iter::Peekable;

/// The building blocks for more complex language constructs. A keyword is any valid identifier 
/// (see: [identifier_start] and [identifier_body]) reserved for use by the language.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    /// `print`
    Print,
    /// `set`
    Set,
    /// `var`
    Var,
    /// `call`
    Call,
    /// `return`
    Return,
    /// `do`
    Do,
    /// `?loop`
    LoopIf,
    /// `?while`
    While,
    /// `func`
    Func,
    /// `does`
    Does,
    /// `done`
    Done,
    /// `type`
    Type,
    /// `print-type`
    PrintType,
    /// `as`
    As,
}

/// Binary operators
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    /// Addition
    Plus,
    /// Subtracton
    Hyphen,
    /// Multiplication
    Asterisk,
    /// Division
    Slash,
    /// Equality testing
    Equals,

    /// Logical and 
    And,
    /// Logical or
    Or
}

/// Unary operators
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
    /// Logical not
    Not
}

/// A [TokenKind] tells us what the token represents, e.g. if the parser encounters a token with 
/// the kind `StringLit`, then it can handle it accordingly.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    /// Fundamental building blocks of the language
    Keyword(Keyword),

    /// See: [BinOp]
    BinOp(BinOp),
    /// See: [UnOp]
    UnOp(UnOp),

    /// A string literal starting with a quotation mark, containing any number of UTF-8 characters 
    /// and escape sequences until an unescaped quotation mark is encountered.
    StringLit,
    /// Any string of characters uninterrupted by whitespace and not already categorized otherwise.
    Identifier,
    /// A decimal integer
    Integer,
    /// See: [char::is_whitespace]
    Whitespace,
    /// A comment beginning with a hashtag and ending at the end of the line or EOF
    Comment
}

/// A [Token] describes a chunk of the source file, e.g. if the lexer encounters an
/// integer, then it will produce a [Token] with `kind` [TokenKind::Integer] and a span 
/// pointing to where the [Token] originates.
#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    /// The described piece of text extracted from the source string
    data: &'a str,
    /// The start and end postion, in bytes, of the token in the source file
    span: (usize, usize),
    /// The type of the token
    kind: TokenKind
}

impl<'a> Token<'a> {
    pub fn data(&self) -> &'a str        { self.data }
    pub fn span(&self) -> (usize, usize) { self.span }
    pub fn kind(&self) -> TokenKind      { self.kind }
}

/// An [Iterator] that produces a stream of tokens describing a source string. The lexer does not 
/// perform any memory allocations or copying of data, it simply scans the source file, determines
/// the type of the next token, and takes an immutable view into the source data.
/// 
/// # Example
/// ```rust
/// const TEST_SRC: &str = "abc 123";
/// 
/// let mut lx = Lexer::from_source(TEST_SRC);
/// 
/// assert_eq!(lx.next_token().map(|t| t.kind()), Some(TokenKind::Identifier));
/// assert_eq!(lx.next_token().map(|t| t.kind()), Some(TokenKind::Integer));
/// ```
pub struct Lexer<'a> {
    /// The source string being tokenised, used to extract a [Token]'s `data`
    source: &'a str,
    /// A stream of characters and their byte index in the source string.
    stream: Peekable<CharIndices<'a>>,
    /// The next `Token` the lexer will encounter 
    peek: Option<Token<'a>>
}

impl<'a> Lexer<'a> {
    /// Create a new [Lexer] and initialise it with a file's contents.
    pub fn from_source(source: &'a str) -> Lexer<'a> {
        let mut lx = Lexer { 
            source: source, 
            stream: source.char_indices().peekable(), 
            peek: None
        };

        // advance the lexer once to fill the `peek` value
        lx.peek = lx.advance();

        lx
    }
}

/// A valid identifier starts with a non-whitespace, non-numeric, non-control Unicode glyph.
fn identifier_start(c: char) -> bool {
    !c.is_whitespace() && !c.is_numeric() && !c.is_control()
}

/// A valid identifier body comprises non-whitespace, non-control Unicode glyphs
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

            '"'         => self.string_lit(),
            c if identifier_start(*c) => self.identifier(),
            '0' ..= '9' => self.integer(),

            c => unimplemented!("Unhandled case: '{}'", c)
        }
    }
}

impl<'a> Lexer<'a> {
    /// Tokenize a sequence of whitespace characters
    fn whitespace(&mut self) -> Option<Token<'a>> {
        let (start,_) = self.stream.next()?;
        let mut end = start + 1;

        // while next char is whitespace, consume and update span
        while let Some((pos,_)) = self.stream.next_if(|(_,c)| c.is_whitespace()) {
            end = pos + 1;
        }

        let data = &self.source[start..end];

        Some(Token {
            data,
            span: (start, end),
            kind: TokenKind::Whitespace
        })
    }

    /// Tokenize a comment
    fn comment(&mut self) -> Option<Token<'a>> {
        let (start,_) = self.stream.next()?;
        let mut end = start + 1;

        // consume stream until EOL or EOF encountered
        while let Some((pos,_)) = self.stream.next_if(|&(_,c)| c != '\n') {
            end = pos + 1;
        }

        let data = &self.source[start..end];

        Some(Token {
            data,
            span: (start, end),
            kind: TokenKind::Comment
        })
    }

    /// Tokenize an identifier
    fn identifier(&mut self) -> Option<Token<'a>> {
        let (start, _) = self.stream.next()?;
        let mut end = start + 1;

        // while next char is valid ident body, consume and update span
        while let Some((pos,_)) = self.stream.next_if(|(_,c)| identifier_body(*c)) {
            end = pos + 1;
        }

        let data = &self.source[start..end];
        
        // determine if token is an op, keyword, or identifier
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

            "and" => TokenKind::BinOp(BinOp::And),
            "or" => TokenKind::BinOp(BinOp::Or),
            
            "not" => TokenKind::UnOp(UnOp::Not),

            _ => TokenKind::Identifier
        };

        Some(Token {
            data,
            span: (start, end),
            kind
        })
    }

    /// Tokenize an integer
    fn integer(&mut self) -> Option<Token<'a>> {
        let (start, _) = self.stream.next()?;
        let mut end = start + 1;

        // while next char is digit, consume and update span
        while let Some((pos,_)) = self.stream.next_if(|(_,c)| matches!(c, '0' ..= '9')) {
            end = pos + 1;
        }

        let data = &self.source[start..end];

        Some(Token {
            data,
            span: (start, end),
            kind: TokenKind::Integer
        })
    }

    /// Tokenize a string literal
    fn string_lit(&mut self) -> Option<Token<'a>> {
        let (start,_) = self.stream.next()?;
        let mut end = start + 1;

        // consume stream until quotation mark encountered
        // TODO: handle escape sequences
        while let Some((pos,_)) = self.stream.next_if(|&(_,c)| c != '"') {
            end = pos + 1;
        }

        // consume finishing quotation mark
        if matches!(self.stream.peek(), Some((_,'"'))) {
            end += 1;
            self.stream.next();
        }

        let data = &self.source[start..end];

        Some(Token {
            data,
            span: (start, end),
            kind: TokenKind::StringLit
        })
    }
}

impl<'a> Lexer<'a> {
    /// Advances the [Lexer], ignoring whitespace and comments.
    fn advance(&mut self) -> Option<Token<'a>> {
        let next = self.next()?;

        if matches!(next.kind(), TokenKind::Whitespace | TokenKind::Comment) {
            self.advance()
        } else {
            Some(next)
        }
    }

    /// Check the value stored in `peek` without advancing the [Lexer].
    pub fn peek_token(&mut self) -> Option<Token<'a>> {
        self.peek
    }

    /// Advance the `Lexer` and update the `peek`ed value.
    pub fn next_token(&mut self) -> Option<Token<'a>> {
        let t = self.peek_token();
        self.peek = self.advance();
        t
    }
}
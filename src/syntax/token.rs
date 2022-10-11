use std::fmt;

use crate::span::{Span, WithSpan};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TokenKind {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Dot,    // .
    Comma,  // ,
    Semi,   // ;
    Colon,  // :
    Equals, // =

    And,    // and
    Or,     // or
    Not,    // not
    Is,     // is
    Plus,   // +
    Minus,  // -
    Star,   // *
    Slash,  // /
    Mod,    // %
    EqEq,   // ==
    BangEq, // !=
    Lt,     // <
    LeEq,   // <=
    Gt,     // >
    GtEq,   // >=
    Pipe,   // |>
    Dot2,   // ..
    Dot3,   // ...

    Lower,
    Upper,
    Number,
    String,

    StringStart,
    StringEnd,
    StringSlice,

    NewLine,
    Eof,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenKind::Eof,
            span: Span(0, 0),
        }
    }
}

impl WithSpan for Token {
    fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Dot2 => write!(f, ".."),
            TokenKind::Dot3 => write!(f, "..."),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semi => write!(f, ";"),
            TokenKind::Equals => write!(f, "="),
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Not => write!(f, "not"),
            TokenKind::Is => write!(f, "is"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Mod => write!(f, "%"),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::BangEq => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::LeEq => write!(f, "<="),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::Pipe => write!(f, "|>"),
            TokenKind::Lower => write!(f, "lowercase name"),
            TokenKind::Upper => write!(f, "uppercase name"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::String => write!(f, "string"),
            TokenKind::StringStart => write!(f, "start of string"),
            TokenKind::StringEnd => write!(f, "end of string"),
            TokenKind::StringSlice => write!(f, "slice of string"),
            TokenKind::NewLine => write!(f, "new line"),
            TokenKind::Eof => write!(f, "end of file"),
        }
    }
}

pub fn get_keyword(key: &str) -> Option<TokenKind> {
    match key {
        "and" => Some(TokenKind::And),
        "or"  => Some(TokenKind::Or),
        "not" => Some(TokenKind::Not),
        "is"  => Some(TokenKind::Is),
        _ => None,
    }
}

impl Token {
    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn terminator(&self) -> bool {
        matches!(self.kind,
            TokenKind::RParen
          | TokenKind::RBrace
          | TokenKind::RBracket
          | TokenKind::Comma
          | TokenKind::NewLine
        )
    }
}

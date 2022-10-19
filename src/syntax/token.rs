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

    Op(OpKind),
    Id(IdKind),
    Number,
    String,

    StringStart,
    StringEnd,
    StringSlice,

    NewLine,
    Eof,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum OpKind {
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
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum IdKind {
    Upper,
    Lower,
    AllCaps,
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
            TokenKind::Comma => write!(f, ","),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semi => write!(f, ";"),
            TokenKind::Equals => write!(f, "="),
            TokenKind::Id(id) => write!(f, "{}", id),
            TokenKind::Op(op) => write!(f, "{}", op),
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

impl fmt::Display for OpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpKind::And => write!(f, "and"),
            OpKind::Or => write!(f, "or"),
            OpKind::Not => write!(f, "not"),
            OpKind::Is => write!(f, "is"),
            OpKind::Plus => write!(f, "+"),
            OpKind::Minus => write!(f, "-"),
            OpKind::Star => write!(f, "*"),
            OpKind::Slash => write!(f, "/"),
            OpKind::Mod => write!(f, "%"),
            OpKind::EqEq => write!(f, "=="),
            OpKind::BangEq => write!(f, "!="),
            OpKind::Lt => write!(f, "<"),
            OpKind::LeEq => write!(f, "<="),
            OpKind::Gt => write!(f, ">"),
            OpKind::GtEq => write!(f, ">="),
            OpKind::Pipe => write!(f, "|>"),
        }
    }
}

impl fmt::Display for IdKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IdKind::Lower => write!(f, "lowercase identifier"),
            IdKind::Upper => write!(f, "titlecase identifier"),
            IdKind::AllCaps => write!(f, "all caps identifier"),
        }
    }
}

pub fn get_keyword(key: &str) -> Option<TokenKind> {
    match key {
        "and" => Some(TokenKind::Op(OpKind::And)),
        "or"  => Some(TokenKind::Op(OpKind::Or)),
        "not" => Some(TokenKind::Op(OpKind::Not)),
        "is"  => Some(TokenKind::Op(OpKind::Is)),
        _ => None,
    }
}

impl Token {
    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn is_terminator(&self) -> bool {
        matches!(self.kind,
            TokenKind::RParen
          | TokenKind::RBrace
          | TokenKind::RBracket
          | TokenKind::Comma
          | TokenKind::NewLine
        )
    }
}

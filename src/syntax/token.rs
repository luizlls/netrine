use std::fmt;

use crate::span::Span;

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
    Walrus, // :=

    And,    // and
    Or,     // or
    Not,    // not
    Is,     // is
    Plus,   // +
    Minus,  // -
    Star,   // *
    Slash,  // /
    Caret,  // ^
    Mod,    // %
    EqEq,   // ==
    NoEq,   // !=
    Lt,     // <
    LtEq,   // <=
    Gt,     // >
    GtEq,   // >=
    Pipe,   // |>

    Ident,
    Number,
    String,

    StringStart,
    StringPart,
    StringEnd,

    Error(TokenErrorKind),

    NewLine,
    EOF,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TokenErrorKind {
    UnexpectedCharacter,
    UnterminatedString,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenKind::EOF,
            span: Span(0, 0),
        }
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
            TokenKind::Walrus => write!(f, ":="),
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Not => write!(f, "not"),
            TokenKind::Is => write!(f, "is"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Mod => write!(f, "%"),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::NoEq => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::Pipe => write!(f, "|>"),
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::String => write!(f, "string"),
            TokenKind::StringStart => write!(f, "start of a string"),
            TokenKind::StringEnd => write!(f, "end of a string"),
            TokenKind::StringPart => write!(f, "part of a string"),
            TokenKind::Error(err) => write!(f, "{}", err),
            TokenKind::NewLine => write!(f, "new line"),
            TokenKind::EOF => write!(f, "end of file"),
        }
    }
}

impl fmt::Display for TokenErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenErrorKind::UnexpectedCharacter => write!(f, "unexpected character"),
            TokenErrorKind::UnterminatedString => write!(f, "unterminated string"),
        }
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

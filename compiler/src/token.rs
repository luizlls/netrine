use std::fmt;

use crate::source::{Span, ToSpan};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub enum TokenKind {
    #[default]
    EOF,
    EOL,

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
    Arrow,  // =>

    Let,
    And,
    Or,
    Not,

    Plus,  // +
    Minus, // -
    Star,  // *
    Slash, // /
    Mod,   // %
    Caret, // ^
    EqEq,  // ==
    NoEq,  // !=
    Lt,    // <
    LtEq,  // <=
    Gt,    // >
    GtEq,  // >=
    Dots,  // ..

    Identifier,
    Underscore,
    Number,
    Integer,
    String,

    UnexpectedCharacter,
    UnterminatedString,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let description = match self {
            TokenKind::EOF => "end of input",
            TokenKind::EOL => "new line",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::LBracket => "[",
            TokenKind::RBracket => "]",
            TokenKind::Dot => ".",
            TokenKind::Comma => ",",
            TokenKind::Colon => ":",
            TokenKind::Semi => ";",
            TokenKind::Equals => "=",
            TokenKind::Arrow => "=>",
            TokenKind::Let => "let",
            TokenKind::And => "and",
            TokenKind::Or => "or",
            TokenKind::Not => "not",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Caret => "^",
            TokenKind::Mod => "%",
            TokenKind::EqEq => "==",
            TokenKind::NoEq => "!=",
            TokenKind::Lt => "<",
            TokenKind::LtEq => "<=",
            TokenKind::Gt => ">",
            TokenKind::GtEq => ">=",
            TokenKind::Dots => "..",
            TokenKind::Identifier => "identifier",
            TokenKind::Underscore => "_",
            TokenKind::Number => "number",
            TokenKind::Integer => "integer",
            TokenKind::String => "string",
            TokenKind::UnexpectedCharacter => "unexpected character",
            TokenKind::UnterminatedString => "unterminated string",
        };
        write!(f, "{description}")
    }
}

impl Token {
    #[rustfmt::skip]
    pub fn non_terminal(self) -> bool {
        matches!(
            self.kind,
          | TokenKind::Dot
          | TokenKind::Plus
          | TokenKind::Minus
          | TokenKind::Star
          | TokenKind::Slash
          | TokenKind::Mod
          | TokenKind::Caret
          | TokenKind::EqEq
          | TokenKind::NoEq
          | TokenKind::Lt
          | TokenKind::LtEq
          | TokenKind::Gt
          | TokenKind::GtEq
          | TokenKind::Dots
        )
    }

    pub fn is(self, kind: TokenKind) -> bool {
        self.kind == kind
    }
}

impl ToSpan for Token {
    fn span(&self) -> Span {
        self.span
    }
}

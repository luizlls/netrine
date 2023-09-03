use std::fmt;

use crate::span::{IntoSpan, Span};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub enum TokenKind {
    #[default]
    EOF,

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

    And,
    Or,
    Not,
    Is,
    Mut,
    If,
    Else,
    Match,
    Case,
    Yield,
    Return,
    Where,
    Import,

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

    Ident,
    Number,
    String,

    UnexpectedCharacter,
    UnterminatedString,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let description = match self {
            TokenKind::EOF => "end of input",
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
            TokenKind::And => "and",
            TokenKind::Or => "or",
            TokenKind::Is => "is",
            TokenKind::Mut => "mut",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Match => "match",
            TokenKind::Case => "case",
            TokenKind::Not => "not",
            TokenKind::Yield => "yield",
            TokenKind::Return => "return",
            TokenKind::Where => "where",
            TokenKind::Import => "import",
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
            TokenKind::Ident => "identifier",
            TokenKind::Number => "number",
            TokenKind::String => "string",
            TokenKind::UnexpectedCharacter => "unexpected character",
            TokenKind::UnterminatedString => "unterminated string",
        };
        write!(f, "{description}")
    }
}

impl TokenKind {
    pub fn is(self, kind: TokenKind) -> bool {
        self == kind
    }
}

impl IntoSpan for Token {
    fn span(&self) -> Span {
        self.span
    }
}

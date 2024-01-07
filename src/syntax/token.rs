use std::fmt;

use crate::span::Span;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub enum TokenKind {
    #[default]
    EOF,
    NL,

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
    For,
    In,
    Case,
    Where,
    Import,
    Yield,
    Break,
    Return,

    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Mod,     // %
    Caret,   // ^
    EqEq,    // ==
    NoEq,    // !=
    Lt,      // <
    LtEq,    // <=
    Gt,      // >
    GtEq,    // >=
    Dots,    // ..

    Ident,
    Number,
    Integer,
    String,

    UnexpectedCharacter,
    UnterminatedString,
    Noop,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let description = match self {
            TokenKind::EOF => "end of input",
            TokenKind::NL => "new line",
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
            TokenKind::Not => "not",
            TokenKind::Is => "is",
            TokenKind::Mut => "mut",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::For => "for",
            TokenKind::In => "in",
            TokenKind::Case => "case",
            TokenKind::Where => "where",
            TokenKind::Import => "import",
            TokenKind::Yield => "yield",
            TokenKind::Break => "break",
            TokenKind::Return => "return",
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
            TokenKind::Integer => "integer",
            TokenKind::String => "string",
            TokenKind::UnexpectedCharacter => "unexpected character",
            TokenKind::UnterminatedString => "unterminated string",
            TokenKind::Noop => "placeholder",
        };
        write!(f, "{description}")
    }
}

impl Token {
    pub fn is(self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn non_terminal(self) -> bool {
        matches!(
            self.kind,
            TokenKind::Dot
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
}

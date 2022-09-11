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
    Colon,  // :
    Semi,   // ;
    Equals, // =

    And,   // and
    Or,    // or
    Not,   // not
    Is,    // is
    Add,   // +
    Sub,   // -
    Mul,   // *
    Div,   // /
    Rem,   // %
    Eq,    // ==
    Ne,    // !=
    Lt,    // <
    Le,    // <=
    Gt,    // >
    Ge,    // >=
    Pipe,  // |>
    Range, // ..

    Lower,
    Upper,
    Number,
    String,

    StringStart,
    StringEnd,
    StringSlice,

    EOF,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenKind::EOF,
            span: Span { line: 0, start: 0, end: 0 },
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
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Not => write!(f, "not"),
            TokenKind::Is => write!(f, "is"),
            TokenKind::Add => write!(f, "+"),
            TokenKind::Sub => write!(f, "-"),
            TokenKind::Mul => write!(f, "*"),
            TokenKind::Div => write!(f, "/"),
            TokenKind::Rem => write!(f, "%"),
            TokenKind::Eq => write!(f, "=="),
            TokenKind::Ne => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Le => write!(f, "<="),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::Ge => write!(f, ">="),
            TokenKind::Pipe => write!(f, "|>"),
            TokenKind::Range => write!(f, ".."),
            TokenKind::Lower => write!(f, "lowercase name"),
            TokenKind::Upper => write!(f, "uppercase name"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::String => write!(f, "string"),
            TokenKind::StringStart => write!(f, "start of string"),
            TokenKind::StringEnd => write!(f, "end of string"),
            TokenKind::StringSlice => write!(f, "slice of string"),
            TokenKind::EOF => write!(f, "end of file"),
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

pub type Precedence = u8;

impl Token {
    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn is_operator(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::And
          | TokenKind::Or
          | TokenKind::Is
          | TokenKind::Add
          | TokenKind::Sub
          | TokenKind::Mul
          | TokenKind::Div
          | TokenKind::Rem
          | TokenKind::Eq
          | TokenKind::Ne
          | TokenKind::Lt
          | TokenKind::Le
          | TokenKind::Gt
          | TokenKind::Ge
          | TokenKind::Pipe
          | TokenKind::Range
        )
    }

    pub fn precedence(&self) -> Option<Precedence> {
        let precedence = match self.kind {
            TokenKind::Div   => 7,
            TokenKind::Mul   => 7,
            TokenKind::Rem   => 7,
            TokenKind::Add   => 6,
            TokenKind::Sub   => 6,
            TokenKind::Lt    => 5,
            TokenKind::Le    => 5,
            TokenKind::Gt    => 5,
            TokenKind::Ge    => 5,
            TokenKind::Ne    => 5,
            TokenKind::Eq    => 5,
            TokenKind::Is    => 5,
            TokenKind::And   => 4,
            TokenKind::Or    => 3,
            TokenKind::Range => 2,
            TokenKind::Pipe  => 1,
            _ => return None,
        };
        Some(precedence)
    }
}

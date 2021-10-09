use std::fmt;

use crate::Span;

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    Hash,   // #
    Equals, // =

    Match,
    Case,
    Then,
    Else,
    Do,
    End,

    And,   // and
    Or,    // or
    Not,   // not
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

    Error(TokenError),

    EOF, 
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
            TokenKind::Hash => write!(f, "#"),
            TokenKind::Equals => write!(f, "="),
            TokenKind::Match => write!(f, "match"),
            TokenKind::Case => write!(f, "case"),
            TokenKind::Then => write!(f, "then"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Do => write!(f, "do"),
            TokenKind::End => write!(f, "end"),
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Not => write!(f, "not"),
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
            TokenKind::Lower => write!(f, "identifier"),
            TokenKind::Upper => write!(f, "constructor"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::String => write!(f, "string"),
            TokenKind::StringStart => write!(f, "start of string template"),
            TokenKind::StringEnd => write!(f, "end of string template"),
            TokenKind::StringSlice => write!(f, "slice of string template"),
            TokenKind::EOF => write!(f, "end of file"),
            TokenKind::Error(err) => write!(f, "{}", err),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenError {
    InvalidCharacter,
    InvalidOperator,
    InvalidEscapeCharacter,
    UnterminatedString,
    UnterminatedIdentifier,
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenError::InvalidCharacter => write!(f, "invalid character"),
            TokenError::InvalidEscapeCharacter => write!(f, "invalid escape"),
            TokenError::InvalidOperator => write!(f, "invalid operator"),
            TokenError::UnterminatedString => write!(f, "unterminated string"),
            TokenError::UnterminatedIdentifier => write!(f, "unterminated identifier"),
        }
    }
}

impl Default for TokenKind {
    fn default() -> Self {
        TokenKind::EOF
    }
}

pub fn get_keyword(key: &str) -> Option<TokenKind> {
    match key {
        "and"   => Some(TokenKind::And),
        "or"    => Some(TokenKind::Or),
        "not"   => Some(TokenKind::Not),
        "match" => Some(TokenKind::Match),
        "case"  => Some(TokenKind::Case),
        "then"  => Some(TokenKind::Then),
        "else"  => Some(TokenKind::Else),
        "do"    => Some(TokenKind::Do),
        "end"   => Some(TokenKind::End),
        _ => None,
    }
}

pub type Precedence = u8;

impl Token {

    pub fn is_operator(&self) -> bool {
        matches!(self.kind,
            TokenKind::And
          | TokenKind::Or
          | TokenKind::Not
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

    pub fn is_delimiter(&self) -> bool {
        matches!(self.kind,
            TokenKind::LParen
          | TokenKind::RParen
          | TokenKind::LBrace
          | TokenKind::RBrace
          | TokenKind::LBracket
          | TokenKind::RBracket
          | TokenKind::Comma
          | TokenKind::End
          | TokenKind::Case
          | TokenKind::Then
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
            TokenKind::And   => 4,
            TokenKind::Or    => 3,
            TokenKind::Range => 2,
            TokenKind::Pipe  => 1,
            _ => return None
        };
        Some(precedence)
    }
}

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

    Dot,        // .
    Comma,      // ,
    Colon,      // :
    Semi,       // ;
    Arrow,      // =>
    Equals,     // =
    Warlus,     // :=
    Underscore, // _

    If,
    Then,
    Else,
    Do,
    End,
    
    Add,  // +
    Sub,  // -
    Mul,  // *
    Div,  // /
    Rem,  // %
    And,  // and
    Or,   // or
    Is,   // is
    Not,  // not
    Eq,   // ==
    Ne,   // !=
    Lt,   // <
    Le,   // <=
    Gt,   // >
    Ge,   // >=
    Pipe, // |>
    Range, // ..

    Lower,
    Upper,
    Number,
    String,

    StringStart,
    StringEnd,
    StringSlice,

    Error(TokenError),

    EOS,    
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
            TokenKind::Semi  => write!(f, ";"),
            TokenKind::Arrow => write!(f, "=>"),
            TokenKind::Equals => write!(f, "="),
            TokenKind::Warlus => write!(f, ":="),
            TokenKind::Underscore => write!(f, "_"),

            TokenKind::If => write!(f, "if"),
            TokenKind::Then => write!(f, "then"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Do => write!(f, "do"),
            TokenKind::End => write!(f, "end"),

            TokenKind::And => write!(f, "and"),
            TokenKind::Or  => write!(f, "or"),
            TokenKind::Is  => write!(f, "is"),
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
            TokenKind::Pipe  => write!(f, "|>"),
            TokenKind::Range => write!(f, ".."),

            TokenKind::Lower => write!(f, "lowercase identifier"),
            TokenKind::Upper => write!(f, "uppercase identifier"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::String => write!(f, "string"),

            TokenKind::StringStart => write!(f, "start of string template"),
            TokenKind::StringEnd => write!(f, "end of string template"),
            TokenKind::StringSlice => write!(f, "slice of string template"),

            TokenKind::Error(error) => write!(f, "{}", error),

            TokenKind::EOS => write!(f, "end of source")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenError {
    InvalidCharacter,
    InvalidOperator,
    InvalidEscapeCharacter,
    UnterminatedString,
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenError::InvalidCharacter => write!(f, "invalid character"),
            TokenError::InvalidEscapeCharacter => write!(f, "invalid escape"),
            TokenError::InvalidOperator => write!(f, "invalid operator"),
            TokenError::UnterminatedString => write!(f, "unterminated string"),
        }
    }
}

impl Default for TokenKind {
    fn default() -> Self {
        TokenKind::EOS
    }
}

pub fn get_keyword(key: &str) -> Option<TokenKind> {
    match key {
        "if"   => Some(TokenKind::If),
        "then" => Some(TokenKind::Then),
        "else" => Some(TokenKind::Else),
        "and"  => Some(TokenKind::And),
        "do"   => Some(TokenKind::Do),
        "end"  => Some(TokenKind::End),
        "or"   => Some(TokenKind::Or),
        "is"   => Some(TokenKind::Is),
        "not"  => Some(TokenKind::Not),
        _ => None,
    }
}

pub fn get_operator(key: &str) -> Option<TokenKind> {
    match key {
        ":"  => Some(TokenKind::Colon),
        "."  => Some(TokenKind::Dot),
        "="  => Some(TokenKind::Equals),
        ":=" => Some(TokenKind::Warlus),
        "=>" => Some(TokenKind::Arrow),
        "+"  => Some(TokenKind::Add),
        "-"  => Some(TokenKind::Sub),
        "*"  => Some(TokenKind::Mul),
        "/"  => Some(TokenKind::Div),
        "%"  => Some(TokenKind::Rem),
        "==" => Some(TokenKind::Eq),
        "!=" => Some(TokenKind::Ne),
        "<"  => Some(TokenKind::Lt),
        "<=" => Some(TokenKind::Le),
        ">"  => Some(TokenKind::Gt),
        ">=" => Some(TokenKind::Ge),
        "|>" => Some(TokenKind::Pipe),
        ".." => Some(TokenKind::Range),
        _ => None
    }
}

pub type Precedence = u8;

impl Token {

    pub fn is_operator(&self) -> bool {
        matches!(self.kind,
            TokenKind::And
          | TokenKind::Or
          | TokenKind::Is
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
          | TokenKind::Semi
        )
    }

    pub fn precedence(&self) -> Option<Precedence> {
        match self.kind {
            TokenKind::Div   => Some(7),
            TokenKind::Mul   => Some(7),
            TokenKind::Rem   => Some(7),
            TokenKind::Add   => Some(6),
            TokenKind::Sub   => Some(6),
            TokenKind::Is    => Some(5),
            TokenKind::Lt    => Some(5),
            TokenKind::Le    => Some(5),
            TokenKind::Gt    => Some(5),
            TokenKind::Ge    => Some(5),
            TokenKind::Ne    => Some(5),
            TokenKind::Eq    => Some(5),
            TokenKind::And   => Some(4),
            TokenKind::Or    => Some(3),
            TokenKind::Range => Some(2),
            TokenKind::Pipe  => Some(1),
            _ => None
        }
    }
}

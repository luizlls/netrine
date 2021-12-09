use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Dot,      // .
    Comma,    // ,
    Colon,    // :
    Semi,     // ;
    Equals,   // =
    Walrus,   // :=
    Arrow,    // =>
    Anything, // _

    Fn,
    If,
    Then,
    Else,

    And,    // and
    Or,     // or
    Not,    // not
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Rem,    // %
    Eq,     // ==
    Ne,     // !=
    Lt,     // <
    Le,     // <=
    Gt,     // >
    Ge,     // >=
    Pipe,   // |>
    Range,  // ..

    Lower,
    Upper,
    Number,
    String,

    StringStart,
    StringEnd,
    StringSlice,

    Indent,
    Dedent,
    NewLine,

    Error(TokenError),

    EOF, 
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::LParen      => write!(f, "("),
            Token::RParen      => write!(f, ")"),
            Token::LBrace      => write!(f, "{{"),
            Token::RBrace      => write!(f, "}}"),
            Token::LBracket    => write!(f, "["),
            Token::RBracket    => write!(f, "]"),
            Token::Dot         => write!(f, "."),
            Token::Comma       => write!(f, ","),
            Token::Colon       => write!(f, ":"),
            Token::Semi        => write!(f, ";"),
            Token::Equals      => write!(f, "="),
            Token::Walrus      => write!(f, ":="),
            Token::Arrow       => write!(f, "=>"),
            Token::Anything    => write!(f, "_"),
            Token::Fn          => write!(f, "fn"),
            Token::If          => write!(f, "if"),
            Token::Then        => write!(f, "then"),
            Token::Else        => write!(f, "else"),
            Token::And         => write!(f, "and"),
            Token::Or          => write!(f, "or"),
            Token::Not         => write!(f, "not"),
            Token::Add         => write!(f, "+"),
            Token::Sub         => write!(f, "-"),
            Token::Mul         => write!(f, "*"),
            Token::Div         => write!(f, "/"),
            Token::Rem         => write!(f, "%"),
            Token::Eq          => write!(f, "=="),
            Token::Ne          => write!(f, "!="),
            Token::Lt          => write!(f, "<"),
            Token::Le          => write!(f, "<="),
            Token::Gt          => write!(f, ">"),
            Token::Ge          => write!(f, ">="),
            Token::Pipe        => write!(f, "|>"),
            Token::Range       => write!(f, ".."),
            Token::Lower       => write!(f, "identifier"),
            Token::Upper       => write!(f, "constructor"),
            Token::Number      => write!(f, "number"),
            Token::String      => write!(f, "string"),
            Token::StringStart => write!(f, "start of string"),
            Token::StringEnd   => write!(f, "end of string"),
            Token::StringSlice => write!(f, "slice of string"),
            Token::Indent      => write!(f, "indentation"),
            Token::Dedent      => write!(f, "dedentation"),
            Token::NewLine     => write!(f, "new line"),
            Token::EOF         => write!(f, "end of file"),
            Token::Error(err)  => write!(f, "{}", err),
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

pub fn get_keyword(key: &str) -> Option<Token> {
    match key {
        "fn"   => Some(Token::Fn),
        "and"  => Some(Token::And),
        "or"   => Some(Token::Or),
        "not"  => Some(Token::Not),
        "if"   => Some(Token::If),
        "then" => Some(Token::Then),
        "else" => Some(Token::Else),
        _ => None,
    }
}

pub type Precedence = u8;

impl Token {

    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            Token::And
          | Token::Or
          | Token::Not
          | Token::Add
          | Token::Sub
          | Token::Mul
          | Token::Div
          | Token::Rem
          | Token::Eq
          | Token::Ne
          | Token::Lt
          | Token::Le
          | Token::Gt
          | Token::Ge
          | Token::Pipe
          | Token::Range
        )
    }

    pub fn is_opening(&self) -> bool {
        matches!(
            self,
            Token::LParen
          | Token::LBrace
          | Token::LBracket
          | Token::Fn
          | Token::If
          | Token::Then
          | Token::Else
        )
    }

    pub fn is_closing(&self) -> bool {
        matches!(
            self,
            Token::RParen
          | Token::RBrace
          | Token::RBracket
          | Token::Comma
          | Token::Fn
          | Token::If
          | Token::Then
          | Token::Else
        )
    }

    pub fn precedence(&self) -> Option<Precedence> {
        let precedence = match self {
            Token::Div   => 7,
            Token::Mul   => 7,
            Token::Rem   => 7,
            Token::Add   => 6,
            Token::Sub   => 6,
            Token::Lt    => 5,
            Token::Le    => 5,
            Token::Gt    => 5,
            Token::Ge    => 5,
            Token::Ne    => 5,
            Token::Eq    => 5,
            Token::And   => 4,
            Token::Or    => 3,
            Token::Range => 2,
            Token::Pipe  => 1,
            _ => return None
        };
        Some(precedence)
    }
}

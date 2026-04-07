use std::fmt;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub enum Token {
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
    True,
    False,
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
    Number,
    Integer,
    String,

    UnexpectedCharacter,
    UnterminatedString,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let description = match self {
            Token::EOF => "end of input",
            Token::EOL => "end of line",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::LBracket => "[",
            Token::RBracket => "]",
            Token::Dot => ".",
            Token::Comma => ",",
            Token::Colon => ":",
            Token::Semi => ";",
            Token::Equals => "=",
            Token::Arrow => "=>",
            Token::Let => "let",
            Token::True => "True",
            Token::False => "False",
            Token::And => "and",
            Token::Or => "or",
            Token::Not => "not",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Caret => "^",
            Token::Mod => "%",
            Token::EqEq => "==",
            Token::NoEq => "!=",
            Token::Lt => "<",
            Token::LtEq => "<=",
            Token::Gt => ">",
            Token::GtEq => ">=",
            Token::Dots => "..",
            Token::Identifier => "identifier",
            Token::Number => "number",
            Token::Integer => "integer",
            Token::String => "string",
            Token::UnexpectedCharacter => "unexpected character",
            Token::UnterminatedString => "unterminated string",
        };
        write!(f, "{description}")
    }
}

impl Token {
    pub fn is(self, other: Token) -> bool {
        self == other
    }
}

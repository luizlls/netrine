use std::fmt;

use crate::span::Span;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NodeKind {
    Node(SyntaxKind, String),
    
    Nodes(SyntaxKind, Vec<Node>),
    
    Token(SyntaxToken),
}

impl Node {
    pub fn node(kind: SyntaxKind, value: String, span: Span) -> Node {
        Node {
            kind: NodeKind::Node(kind, value),
            span,
        }
    }

    pub fn nodes(kind: SyntaxKind, nodes: Vec<Node>, span: Span) -> Node {
        Node {
            kind: NodeKind::Nodes(kind, nodes),
            span,
        }
    }

    pub fn token(token: SyntaxToken, span: Span) -> Node {
        Node {
            kind: NodeKind::Token(token),
            span,
        }
    }

    pub fn error(value: String, span: Span) -> Node {
        Node {
            kind: NodeKind::Node(SyntaxKind::Error, value),
            span,
        }
    }

    pub fn is_trivia(&self) -> bool {
        let NodeKind::Token(token) = self.kind else { return false };
        token.is_trivia()
    }

    pub fn is_token(&self, expected: SyntaxToken) -> bool {
        let NodeKind::Token(token) = self.kind else { return false; };
        token == expected
    }
}

impl Default for Node {
    fn default() -> Node {
        Node {
            kind: NodeKind::Token(SyntaxToken::EOF),
            span: Span(0, 0),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub enum SyntaxKind {
    #[default]
    EOF,

    Let,
    Set,
    Get,
    Call,
    Arguments,
    Unary,
    Binary,
    Group,
    Tuple,
    List,
    Record,
    Property,
    Empty,
    Parameters,
    Lambda,
    LambdaCall,
    Field,
    Spread,
    Variant,
    Pipe,
    TypeAnnotation,

    If,
    Then,
    Else,
    Case,
    Yield,
    Break,
    Import,
    Where,

    Lower,
    Upper,
    Number,
    String,

    Expr,
    Error,
}

impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let description = match self {
            SyntaxKind::EOF => "end of file",
            SyntaxKind::Let => "let",
            SyntaxKind::Set => "set",
            SyntaxKind::Get => "get",
            SyntaxKind::Call => "function call",
            SyntaxKind::Arguments => "function arguments",
            SyntaxKind::Unary => "unary expression",
            SyntaxKind::Binary => "binary expression",
            SyntaxKind::Group => "group",
            SyntaxKind::Tuple => "tuple",
            SyntaxKind::List => "list",
            SyntaxKind::Record => "record",
            SyntaxKind::Property => "record property",
            SyntaxKind::Lambda => "lambda",
            SyntaxKind::LambdaCall => "trailing lambda",
            SyntaxKind::Parameters => "parameters",
            SyntaxKind::Variant => "type variant",
            SyntaxKind::TypeAnnotation => "type annotation",
            SyntaxKind::Pipe => "|>",
            SyntaxKind::Spread => "..",
            SyntaxKind::Empty => "()",
            SyntaxKind::If => "if",
            SyntaxKind::Then => "then",
            SyntaxKind::Else => "else",
            SyntaxKind::Case => "case",
            SyntaxKind::Yield => "yield",
            SyntaxKind::Break => "break",
            SyntaxKind::Where => "where",
            SyntaxKind::Import => "import",
            SyntaxKind::Lower => "lowercase identifier",
            SyntaxKind::Upper => "uppercase identifier",
            SyntaxKind::Number => "number",
            SyntaxKind::String => "string",
            SyntaxKind::Field => "field",
            SyntaxKind::Expr => "expression",
            SyntaxKind::Error => "error",
        };
        write!(f, "{description}")
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub enum SyntaxToken {
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
    Walrus, // :=
    Arrow,  // ->

    If,
    Then,
    Else,
    Case,
    Yield,
    Break,
    Import,
    Where,

    And,    // and
    Or,     // or
    Not,    // not
    Is,     // is
    Pos,    // +
    Neg,    // -
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Exp,    // ^
    Mod,    // %
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

    Space,
    Comment,

    Error,
}

impl fmt::Display for SyntaxToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let description = match self {
            SyntaxToken::EOF => "end of file",
            SyntaxToken::LParen => "(",
            SyntaxToken::RParen => ")",
            SyntaxToken::LBrace => "{",
            SyntaxToken::RBrace => "}",
            SyntaxToken::LBracket => "[",
            SyntaxToken::RBracket => "]",
            SyntaxToken::Dot => ".",
            SyntaxToken::Comma => ",",
            SyntaxToken::Colon => ":",
            SyntaxToken::Semi => ";",
            SyntaxToken::Equals => "=",
            SyntaxToken::Walrus => ":=",
            SyntaxToken::Arrow => "=>",
            SyntaxToken::And => "and",
            SyntaxToken::Or => "or",
            SyntaxToken::Not => "not",
            SyntaxToken::Is => "is",
            SyntaxToken::Pos => "+",
            SyntaxToken::Neg => "-",
            SyntaxToken::Add => "+",
            SyntaxToken::Sub => "-",
            SyntaxToken::Mul => "*",
            SyntaxToken::Div => "/",
            SyntaxToken::Exp => "^",
            SyntaxToken::Mod => "%",
            SyntaxToken::Eq => "==",
            SyntaxToken::Ne => "!=",
            SyntaxToken::Lt => "<",
            SyntaxToken::Le => "<=",
            SyntaxToken::Gt => ">",
            SyntaxToken::Ge => ">=",
            SyntaxToken::Pipe => "|>",
            SyntaxToken::Range => "..",
            SyntaxToken::If => "if",
            SyntaxToken::Then => "then",
            SyntaxToken::Else => "else",
            SyntaxToken::Case => "case",
            SyntaxToken::Yield => "yield",
            SyntaxToken::Break => "break",
            SyntaxToken::Where => "where",
            SyntaxToken::Import => "import",
            SyntaxToken::Lower => "lowercase identifier",
            SyntaxToken::Upper => "uppercase identifier",
            SyntaxToken::Number => "number",
            SyntaxToken::String => "string",
            SyntaxToken::Space => "space",
            SyntaxToken::Comment => "comment",
            SyntaxToken::Error => "error",
        };
        write!(f, "{description}")
    }
}

impl SyntaxToken {
    pub fn is_terminal(self) -> bool {
        matches!(self, SyntaxToken::Semi | SyntaxToken::RBrace | SyntaxToken::Else
                     | SyntaxToken::Case | SyntaxToken::EOF)
    }

    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxToken::Space | SyntaxToken::Comment)
    }
}

pub type Precedence = i8;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    None,
    Left,
    Right,
}

impl SyntaxToken {
    pub fn precedence(self) -> Option<(Precedence, Associativity)> {
        Some(match self {
            SyntaxToken::Pos
          | SyntaxToken::Neg
          | SyntaxToken::Not => (0, Associativity::None),
            SyntaxToken::Exp => (9, Associativity::Right),
            SyntaxToken::Mul
          | SyntaxToken::Div => (8, Associativity::Left),
            SyntaxToken::Add
          | SyntaxToken::Sub => (7, Associativity::Left),
            SyntaxToken::Mod => (6, Associativity::Left),
            SyntaxToken::Lt
          | SyntaxToken::Le
          | SyntaxToken::Gt
          | SyntaxToken::Ge
          | SyntaxToken::Ne
          | SyntaxToken::Eq => (5, Associativity::Left),
            SyntaxToken::Is => (4, Associativity::Left),
            SyntaxToken::And => (3, Associativity::Left),
            SyntaxToken::Or => (2, Associativity::Left),
            SyntaxToken::Range => (1, Associativity::Left),
            _ => {
                return None;
            }
        })
    }
}

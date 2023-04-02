use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SyntaxNode {
    pub kind: SyntaxKind,
    pub size: u32,
    pub value: Value,
}

impl SyntaxNode {
    pub fn basic(kind: SyntaxKind, size: u32) -> SyntaxNode {
        SyntaxNode {
            kind,
            size,
            value: Value::Node(None),
        }
    }

    pub fn node(kind: SyntaxKind, value: String) -> SyntaxNode {
        let size = value.len() as u32;
        SyntaxNode {
            kind,
            size,
            value: Value::Node(Some(value)),
        }
    }

    pub fn error(value: String, size: u32) -> SyntaxNode {
        SyntaxNode {
            kind: SyntaxKind::Error,
            size,
            value: Value::Node(Some(value)),
        }
    }

    pub fn nodes(kind: SyntaxKind, nodes: Vec<SyntaxNode>) -> SyntaxNode {
        let size = nodes.iter().fold(0, |size, node| size + node.size);
        SyntaxNode {
            kind,
            size,
            value: Value::Nodes(nodes),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Node(Option<String>),

    Nodes(Vec<SyntaxNode>),
}

impl Default for SyntaxNode {
    fn default() -> Self {
        SyntaxNode {
            kind: SyntaxKind::EOF,
            size: 0,
            value: Value::Node(Some("EOF".to_string())),
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
    Args,
    Unary,
    Binary,
    Group,
    Tuple,
    List,
    Record,
    Property,
    Empty,
    Lambda,
    Params,
    Field,

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

    Ident,
    Number,
    String,

    Space,
    Comment,

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
            SyntaxKind::Args => "function arguments",
            SyntaxKind::Unary => "unary expression",
            SyntaxKind::Binary => "binary expression",
            SyntaxKind::Group => "group",
            SyntaxKind::Tuple => "tuple",
            SyntaxKind::List => "list",
            SyntaxKind::Record => "record",
            SyntaxKind::Property => "record property",
            SyntaxKind::Lambda => "lambda",
            SyntaxKind::Params => "parameters",
            SyntaxKind::Empty => "()",
            SyntaxKind::LParen => "(",
            SyntaxKind::RParen => ")",
            SyntaxKind::LBrace => "{",
            SyntaxKind::RBrace => "}",
            SyntaxKind::LBracket => "[",
            SyntaxKind::RBracket => "]",
            SyntaxKind::Dot => ".",
            SyntaxKind::Comma => ",",
            SyntaxKind::Colon => ":",
            SyntaxKind::Semi => ";",
            SyntaxKind::Equals => "=",
            SyntaxKind::Walrus => ":=",
            SyntaxKind::Arrow => "=>",
            SyntaxKind::And => "and",
            SyntaxKind::Or => "or",
            SyntaxKind::Not => "not",
            SyntaxKind::Is => "is",
            SyntaxKind::Pos => "+",
            SyntaxKind::Neg => "-",
            SyntaxKind::Add => "+",
            SyntaxKind::Sub => "-",
            SyntaxKind::Mul => "*",
            SyntaxKind::Div => "/",
            SyntaxKind::Exp => "^",
            SyntaxKind::Mod => "%",
            SyntaxKind::Eq => "==",
            SyntaxKind::Ne => "!=",
            SyntaxKind::Lt => "<",
            SyntaxKind::Le => "<=",
            SyntaxKind::Gt => ">",
            SyntaxKind::Ge => ">=",
            SyntaxKind::Pipe => "|>",
            SyntaxKind::Range => "..",
            SyntaxKind::If => "if",
            SyntaxKind::Then => "then",
            SyntaxKind::Else => "else",
            SyntaxKind::Yield => "yield",
            SyntaxKind::Break => "break",
            SyntaxKind::Where => "where",
            SyntaxKind::Import => "import",
            SyntaxKind::Ident => "identifier",
            SyntaxKind::Number => "number",
            SyntaxKind::String => "string",
            SyntaxKind::Field => "field",
            SyntaxKind::Space => "space",
            SyntaxKind::Comment => "comment",
            SyntaxKind::Expr => "expression",
            SyntaxKind::Error => "error",
        };
        write!(f, "{description}")
    }
}

impl SyntaxKind {
    pub fn is_terminal(&self) -> bool {
        matches!(self, SyntaxKind::Semi | SyntaxKind::EOF | SyntaxKind::Else | SyntaxKind::RBrace)
    }

    pub fn is_trivia(&self) -> bool {
        matches!(self, SyntaxKind::Space | SyntaxKind::Comment)
    }
}

pub type Precedence = i8;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    None,
    Left,
    Right,
}

impl SyntaxKind {
    pub fn precedence(self) -> Option<(Precedence, Associativity)> {
        Some(match self {
            SyntaxKind::Pos
          | SyntaxKind::Neg
          | SyntaxKind::Not => (0, Associativity::None),
            SyntaxKind::Exp => (9, Associativity::Right),
            SyntaxKind::Mul
          | SyntaxKind::Div => (8, Associativity::Left),
            SyntaxKind::Add
          | SyntaxKind::Sub => (7, Associativity::Left),
            SyntaxKind::Mod => (6, Associativity::Left),
            SyntaxKind::Lt
          | SyntaxKind::Le
          | SyntaxKind::Gt
          | SyntaxKind::Ge
          | SyntaxKind::Ne
          | SyntaxKind::Eq => (5, Associativity::Left),
            SyntaxKind::Is => (4, Associativity::Left),
            SyntaxKind::And => (3, Associativity::Left),
            SyntaxKind::Or => (2, Associativity::Left),
            SyntaxKind::Range => (1, Associativity::Left),
            _ => {
                return None;
            }
        })
    }
}

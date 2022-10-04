use crate::span::{Span, WithSpan};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Node {
    Id(Identifier),

    Define(Box<Define>),

    Call(Box<Call>),

    Unary(Box<Unary>),

    Binary(Box<Binary>),

    Partial(Box<Partial>),

    Number(Literal),

    String(Literal),

    True(Span),

    False(Span),
}

impl WithSpan for Node {
    fn span(&self) -> Span {
        match self {
            Node::Id(node) => node.span,
            Node::Define(node) => node.span,
            Node::Call(node) => node.span,
            Node::Unary(node) => node.span,
            Node::Binary(node) => node.span,
            Node::Partial(node) => node.span,
            Node::Number(node) => node.span,
            Node::String(node) => node.span,
            Node::True(span) => *span,
            Node::False(span) => *span,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    pub value: String,
    pub span: Span,
}

impl WithSpan for Identifier {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

impl WithSpan for Literal {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Define {
    pub name: Identifier,
    pub value: Node,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Call {
    pub function: Node,
    pub arguments: Vec<Argument>,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Argument {
    pub value: Node,
    pub name: Option<Identifier>,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Unary {
    pub operator: Operator,
    pub rhs: Node,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Binary {
    pub operator: Operator,
    pub lhs: Node,
    pub rhs: Node,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Partial {
    pub operator: Operator,
    pub lhs: Option<Node>,
    pub rhs: Option<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum OperatorKind {
    Add,
    Pos,
    Sub,
    Neg,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Not,
    Is,
    Pipe,
    Range,
}

pub type Precedence = u8;

impl Operator {
    pub fn unary(&self) -> bool {
        matches!(self.kind,
            OperatorKind::Not
          | OperatorKind::Pos
          | OperatorKind::Neg)
    }

    pub fn precedence(&self) -> Precedence {
        match self.kind {
            OperatorKind::Not   => 0,
            OperatorKind::Neg   => 0,
            OperatorKind::Pos   => 0,
            OperatorKind::Mul   => 7,
            OperatorKind::Div   => 7,
            OperatorKind::Mod   => 7,
            OperatorKind::Add   => 6,
            OperatorKind::Sub   => 6,
            OperatorKind::Lt    => 5,
            OperatorKind::Le    => 5,
            OperatorKind::Gt    => 5,
            OperatorKind::Ge    => 5,
            OperatorKind::Ne    => 5,
            OperatorKind::Eq    => 5,
            OperatorKind::Is    => 5,
            OperatorKind::And   => 4,
            OperatorKind::Or    => 3,
            OperatorKind::Range => 2,
            OperatorKind::Pipe  => 1,
        }
    }
}

impl WithSpan for Operator {
    fn span(&self) -> Span {
        self.span
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            OperatorKind::And   => write!(f, "and"),
            OperatorKind::Or    => write!(f, "or"),
            OperatorKind::Not   => write!(f, "not"),
            OperatorKind::Is    => write!(f, "is"),
            OperatorKind::Add   => write!(f, "+"),
            OperatorKind::Sub   => write!(f, "-"),
            OperatorKind::Pos   => write!(f, "+"),
            OperatorKind::Neg   => write!(f, "-"),
            OperatorKind::Mul   => write!(f, "*"),
            OperatorKind::Div   => write!(f, "/"),
            OperatorKind::Mod   => write!(f, "%"),
            OperatorKind::Eq    => write!(f, "=="),
            OperatorKind::Ne    => write!(f, "!="),
            OperatorKind::Lt    => write!(f, "<"),
            OperatorKind::Le    => write!(f, "<="),
            OperatorKind::Gt    => write!(f, ">"),
            OperatorKind::Ge    => write!(f, ">="),
            OperatorKind::Pipe  => write!(f, "|>"),
            OperatorKind::Range => write!(f, ".."),
        }
    }
}

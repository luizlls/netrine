use crate::span::{Span, WithSpan};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Node {
    Id(Identifier),

    Op(Operator),

    Define(Box<Define>),

    Apply(Box<Apply>),

    Number(Literal),

    String(Literal),

    True(Span),

    False(Span),
}

impl WithSpan for Node {
    fn span(&self) -> Span {
        match self {
            Node::Id(node) => node.span,
            Node::Op(node) => node.span,
            Node::Define(node) => node.span,
            Node::Apply(node) => node.span,
            Node::Number(node) => node.span,
            Node::String(node) => node.span,
            Node::True(span) => *span,
            Node::False(span) => *span,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Name {
    Id(Identifier),

    Op(Operator),
}


impl WithSpan for Name {
    fn span(&self) -> Span {
        match self {
            Name::Id(id) => id.span,
            Name::Op(op) => op.span,
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
    pub name: Name,
    pub value: Node,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Apply {
    pub values: Vec<Node>,
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
}

pub type Precedence = u8;

impl Operator {
    pub fn is_unary(&self) -> bool {
        matches!(self.kind,
            OperatorKind::Not
          | OperatorKind::Pos
          | OperatorKind::Neg)
    }

    pub fn precedence(&self) -> Precedence {
        match self.kind {
            OperatorKind::Not  => 0,
            OperatorKind::Neg  => 0,
            OperatorKind::Pos  => 0,
            OperatorKind::Mul  => 6,
            OperatorKind::Div  => 6,
            OperatorKind::Mod  => 6,
            OperatorKind::Add  => 5,
            OperatorKind::Sub  => 5,
            OperatorKind::Lt   => 4,
            OperatorKind::Le   => 4,
            OperatorKind::Gt   => 4,
            OperatorKind::Ge   => 4,
            OperatorKind::Ne   => 4,
            OperatorKind::Eq   => 4,
            OperatorKind::Is   => 4,
            OperatorKind::And  => 3,
            OperatorKind::Or   => 2,
            OperatorKind::Pipe => 1,
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
            OperatorKind::And  => write!(f, "and"),
            OperatorKind::Or   => write!(f, "or"),
            OperatorKind::Not  => write!(f, "not"),
            OperatorKind::Is   => write!(f, "is"),
            OperatorKind::Add  => write!(f, "+"),
            OperatorKind::Sub  => write!(f, "-"),
            OperatorKind::Pos  => write!(f, "+"),
            OperatorKind::Neg  => write!(f, "-"),
            OperatorKind::Mul  => write!(f, "*"),
            OperatorKind::Div  => write!(f, "/"),
            OperatorKind::Mod  => write!(f, "%"),
            OperatorKind::Eq   => write!(f, "=="),
            OperatorKind::Ne   => write!(f, "!="),
            OperatorKind::Lt   => write!(f, "<"),
            OperatorKind::Le   => write!(f, "<="),
            OperatorKind::Gt   => write!(f, ">"),
            OperatorKind::Ge   => write!(f, ">="),
            OperatorKind::Pipe => write!(f, "|>"),
        }
    }
}

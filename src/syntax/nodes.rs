use crate::span::{Span, ToSpan};

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Identifier(Identifier),

    Fun(Box<Fun>),

    Def(Box<Def>),

    Set(Box<Set>),

    Get(Box<Get>),

    Apply(Box<Apply>),

    Unary(Box<Unary>),

    Binary(Box<Binary>),

    If(Box<If>),

    Block(Box<Block>),

    List(Box<List>),

    Tuple(Box<Tuple>),

    Dict(Box<Dict>),

    Record(Box<Record>),

    Field(Field),

    Number(Literal),

    String(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub value: Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub value: Option<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub target: Node,
    pub value : Node,
    pub mutable: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Set {
    pub target: Node,
    pub value : Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Get {
    pub from: Node,
    pub value: Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Apply {
    pub callee: Node,
    pub arguments: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub nodes: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Node,
    pub rexpr: Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub pred: Node,
    pub then: Node,
    pub otherwise: Option<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub elements: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    pub elements: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dict {
    pub elements: Vec<(Node, Node)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub properties: Vec<(Identifier, Node)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Identifier,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Template {
    pub elements: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub name: Identifier,
    pub value: Node,
    pub span: Span,
}

impl ToSpan for Node {
    fn span(&self) -> Span {
        match &self {
            Node::Identifier(n) => n.span,
            Node::Fun(n) => n.span,
            Node::Def(n) => n.span,
            Node::Set(n) => n.span,
            Node::Get(n) => n.span,
            Node::Apply(n) => n.span,
            Node::Unary(n) => n.span,
            Node::Binary(n) => n.span,
            Node::If(n) => n.span,
            Node::Block(n) => n.span,
            Node::List(n) => n.span,
            Node::Tuple(n) => n.span,
            Node::Dict(n) => n.span,
            Node::Record(n) => n.span,
            Node::Field(n) => n.span,
            Node::Number(n) => n.span,
            Node::String(n) => n.span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OperatorKind {
    Pos,
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Exp,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Is,
    Not,
    Pipe,
    Range,
    Comma,
    Semi,
    Equals,
    Walrus,
}

pub type Precedence = u8;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    None,
    Left,
    Right,
}

impl Operator {
    pub fn is_unary(&self) -> bool {
        matches!(self.kind, OperatorKind::Pos | OperatorKind::Neg | OperatorKind::Not)
    }

    pub fn associativity(&self) -> Associativity {
        match self.kind {
            OperatorKind::Pos
          | OperatorKind::Neg
          | OperatorKind::Not => Associativity::None,
            OperatorKind::Exp
          | OperatorKind::Equals
          | OperatorKind::Walrus => Associativity::Right,
            _ => Associativity::Left,
        }
    }

    pub fn precedence(&self) -> Precedence {
        match self.kind {
            OperatorKind::Pos    => 0,
            OperatorKind::Neg    => 0,
            OperatorKind::Not    => 0,
            OperatorKind::Exp    => 12,
            OperatorKind::Mul    => 11,
            OperatorKind::Div    => 11,
            OperatorKind::Add    => 10,
            OperatorKind::Sub    => 10,
            OperatorKind::Mod    => 9,
            OperatorKind::Lt     => 8,
            OperatorKind::Le     => 8,
            OperatorKind::Gt     => 8,
            OperatorKind::Ge     => 8,
            OperatorKind::Ne     => 8,
            OperatorKind::Eq     => 8,
            OperatorKind::Is     => 8,
            OperatorKind::And    => 7,
            OperatorKind::Or     => 6,
            OperatorKind::Range  => 5,
            OperatorKind::Comma  => 4,
            OperatorKind::Pipe   => 3,
            OperatorKind::Walrus => 2,
            OperatorKind::Equals => 2,
            OperatorKind::Semi   => 1,
        }
    }
}

impl ToSpan for Operator {
    fn span(&self) -> Span {
        self.span
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            OperatorKind::And    => write!(f, "and"),
            OperatorKind::Or     => write!(f, "or"),
            OperatorKind::Not    => write!(f, "not"),
            OperatorKind::Is     => write!(f, "is"),
            OperatorKind::Pos    => write!(f, "+"),
            OperatorKind::Neg    => write!(f, "-"),
            OperatorKind::Add    => write!(f, "+"),
            OperatorKind::Sub    => write!(f, "-"),
            OperatorKind::Mul    => write!(f, "*"),
            OperatorKind::Div    => write!(f, "/"),
            OperatorKind::Exp    => write!(f, "^"),
            OperatorKind::Mod    => write!(f, "%"),
            OperatorKind::Eq     => write!(f, "=="),
            OperatorKind::Ne     => write!(f, "!="),
            OperatorKind::Lt     => write!(f, "<"),
            OperatorKind::Le     => write!(f, "<="),
            OperatorKind::Gt     => write!(f, ">"),
            OperatorKind::Ge     => write!(f, ">="),
            OperatorKind::Pipe   => write!(f, "|>"),
            OperatorKind::Range  => write!(f, ".."),
            OperatorKind::Comma  => write!(f, ","),
            OperatorKind::Semi   => write!(f, ";"),
            OperatorKind::Equals => write!(f, "="),
            OperatorKind::Walrus => write!(f, ":="),
        }
    }
}

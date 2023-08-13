use crate::span::{Span, IntoSpan};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {

    Let(Box<Let>),

    Mut(Box<Mut>),

    Get(Box<Get>),

    Apply(Apply),

    Unary(Box<Unary>),

    Binary(Box<Binary>),

    Field(Box<Field>),

    Group(Box<Group>),

    Block(Block),

    Lambda(Lambda),

    Name(Literal),

    String(Literal),

    Number(Literal),

    List(List),

    Record(Record),

    Tuple(Tuple),

    Empty(Span),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    Unary(Box<Unary>),
    
    Binary(Box<Binary>),
    
    Apply(Apply),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub patt: Node,
    pub value: Option<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Let {
    pub lvalue: Node,
    pub rvalue: Node,
    pub constraints: Vec<Constraint>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mut {
    pub lvalue: Node,
    pub rvalue: Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Get {
    pub node: Node,
    pub field: Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Apply {
    pub nodes: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Group {
    pub node: Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub nodes: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Node,
    pub rexpr: Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    pub parameters: Vec<Parameter>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
    pub predicate: Node,
    pub then: Node,
    pub otherwise: Option<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct List {
    pub items: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub items: Vec<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
    pub properties: Vec<(Name, Node)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub value: Node,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

pub type Name = Literal;

impl Node {
    pub fn is_identifier(&self) -> bool {
        match &self {
            Node::Name(_) => true,
            Node::Apply(Apply { nodes, .. }) => nodes.iter().all(Node::is_identifier),
            _ => false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorKind {
    Is,   // is
    And,  // and
    Or,   // or
    Not,  // not
    Pos,  // +
    Neg,  // -
    Add,  // +
    Sub,  // -
    Mul,  // *
    Div,  // /
    Mod,  // %
    Exp,  // ^
    Eq,   // ==
    Ne,   // !=
    Lt,   // <
    Le,   // <=
    Gt,   // >
    Ge,   // >=
    Range // ..
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

pub type Precedence = i8;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    None,
    Left,
    Right,
}

impl Operator {
    pub fn is_unary(self) -> bool {
        matches!(self.kind, OperatorKind::Pos | OperatorKind::Neg | OperatorKind::Not)
    }

    pub fn precedence(self) -> Precedence {
        match self.kind {
            OperatorKind::Pos
          | OperatorKind::Neg
          | OperatorKind::Not => -1,
            OperatorKind::Exp => 9,
            OperatorKind::Mul
          | OperatorKind::Div => 8,
            OperatorKind::Add
          | OperatorKind::Sub => 7,
            OperatorKind::Mod => 6,
            OperatorKind::Lt
          | OperatorKind::Le
          | OperatorKind::Gt
          | OperatorKind::Ge
          | OperatorKind::Ne
          | OperatorKind::Eq => 5,
            OperatorKind::Is => 4,
            OperatorKind::And => 3,
            OperatorKind::Or => 2,
            OperatorKind::Range => 1,
        }
    }

    pub fn associativity(self) -> Associativity {
        match self.kind {
            OperatorKind::Pos
          | OperatorKind::Neg
          | OperatorKind::Not => Associativity::None,
            OperatorKind::Exp => Associativity::Right,
            _ => Associativity::Left,
        }
    }
}

impl IntoSpan for Node {
    fn span(&self) -> Span {
        match self {
            Node::Let(node) => node.span,
            Node::Mut(node) => node.span,
            Node::Get(node) => node.span,
            Node::Apply(node) => node.span,
            Node::Unary(node) => node.span,
            Node::Binary(node) => node.span,
            Node::Field(node) => node.span,
            Node::Group(node) => node.span,
            Node::Block(node) => node.span,
            Node::Lambda(node) => node.span,
            Node::List(node) => node.span,
            Node::Record(node) => node.span,
            Node::Tuple(node) => node.span,
            Node::Name(node) => node.span,
            Node::String(node) => node.span,
            Node::Number(node) => node.span,
            Node::Empty(span) => *span,

        }
    }
}

impl IntoSpan for Operator {
    fn span(&self) -> Span {
        self.span
    }
}

impl IntoSpan for Literal {
    fn span(&self) -> Span {
        self.span
    }
}

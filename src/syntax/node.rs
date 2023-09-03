use crate::span::{IntoSpan, Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub kind: Box<NodeKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Function(Function),

    Let(Let),

    Mut(Mut),

    Set(Set),

    Access(Access),

    Apply(Apply),

    Unary(Unary),

    Binary(Binary),

    Group(Group),

    Block(Block),

    Lambda(Lambda),

    If(If),

    Match(Match),

    Yield(Yield),

    Return(Return),

    Record(Record),

    List(List),

    Tuple(Tuple),

    Field(Field),

    Name(Name),

    String(Literal),

    Number(Literal),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Let {
    pub kind: LetKind,
    pub lvalue: Node,
    pub rvalue: Node,
    pub constraints: Vec<Constraint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LetKind {
    Equals,
    Arrow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Name,
    pub parameters: Vec<Parameter>,
    pub body: Node,
    pub constraints: Vec<Constraint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    pub paramters: Vec<Parameter>,
    pub body: Node,
    pub constraints: Vec<Constraint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub kind: ConstraintKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintKind {
    Unary(Unary),

    Binary(Binary),

    Apply(Apply),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub patt: Node,
    pub value: Option<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mut {
    pub name: Name,
    pub value: Option<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Set {
    pub node: Node,
    pub value: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Access {
    pub node: Node,
    pub field: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Apply {
    pub callee: Node,
    pub arguments: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Group {
    pub node: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Yield {
    pub value: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Return {
    pub value: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Node,
    pub rexpr: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
    pub value: Node,
    pub then: Node,
    pub otherwise: Option<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Match {
    pub value: Node,
    pub cases: Vec<MatchCase>,
    pub otherwise: Option<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchCase {
    pub case: Node,
    pub then: Node,
    pub guard: Option<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct List {
    pub items: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub items: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
    pub fields: Vec<(Name, Option<Node>)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub value: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

pub type Name = Literal;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorKind {
    Is,    // is
    And,   // and
    Or,    // or
    Not,   // not
    Pos,   // +
    Neg,   // -
    Add,   // +
    Sub,   // -
    Mul,   // *
    Div,   // /
    Mod,   // %
    Exp,   // ^
    Eq,    // ==
    Ne,    // !=
    Lt,    // <
    Le,    // <=
    Gt,    // >
    Ge,    // >=
    Range, // ..
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
            OperatorKind::Pos | OperatorKind::Neg | OperatorKind::Not => -1,
            OperatorKind::Exp => 9,
            OperatorKind::Mul | OperatorKind::Div => 8,
            OperatorKind::Add | OperatorKind::Sub => 7,
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
        self.span
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

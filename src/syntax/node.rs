use crate::span::Span;

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

    Define(Define),

    Assign(Assign),

    Access(Access),

    Mutable(Mutable),

    Apply(Apply),
    
    Accept(Accept),

    Unary(Unary),

    Binary(Binary),

    Group(Group),

    Block(Block),

    Lambda(Lambda),

    Cases(Cases),

    If(If),

    For(For),

    Yield(Yield),

    Break,

    Return(Return),

    Import(Import),

    Record(Record),

    List(List),

    Tuple(Tuple),

    Field(Field),

    Name(Name),

    String(Literal),

    Number(Literal),

    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Define {
    pub lvalue: Node,
    pub rvalue: Node,
    pub constraints: Option<Vec<Node>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Name,
    pub parameters: Vec<Parameter>,
    pub value: Node,
    pub constraints: Option<Vec<Node>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub patt: Node,
    pub value: Option<Node>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    pub parameters: Vec<Node>,
    pub value: Node,
    pub constraints: Option<Vec<Node>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cases {
    pub cases: Vec<Case>,
    pub otherwise: Option<Vec<Node>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Case {
    pub patterns: Vec<Node>,
    pub value: Vec<Node>,
    pub guards: Option<Vec<Node>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mutable {
    pub value: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assign {
    pub operator: Operator,
    pub lvalue: Node,
    pub rvalue: Node,
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
pub struct Accept {
    pub callee: Node,
    pub lambda: Node,
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
    pub expr: Node,
    pub operator: Operator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub lexpr: Node,
    pub rexpr: Node,
    pub operator: Operator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
    pub value: Node,
    pub then: Node,
    pub otherwise: Option<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct For {
    pub bindings: Vec<(Node, Node)>,
    pub value: Node,
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
    pub fields: Vec<RecordField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordField {
    pub kind: RecordFieldKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordFieldKind {
    Field(Name, Option<Node>),
    Spread(Node),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub field: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

pub type Name = Literal;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub module: Name,
    pub names: Vec<Name>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Operator {
    pub kind: OperatorKind,
    pub mode: OperatorMode,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorMode {
    Regular,
    Assign,
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

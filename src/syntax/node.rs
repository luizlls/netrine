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

impl Node {
    pub fn new(kind: NodeKind, span: Span) -> Node {
        Node {
            kind: Box::new(kind),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Function(Function),

    Def(Def),

    Get(Get),

    Set(Set),

    Apply(Apply),

    Unary(Unary),

    Binary(Binary),

    Block(Block),

    Group(Group),

    List(List),

    Tuple(Tuple),

    Name(Name),

    String(Literal),

    Number(Literal),

    Integer(Literal),

    Import(Import),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Name,
    pub parameters: Vec<Parameter>,
    pub value: Node,
}

impl Node {
    pub fn function(name: Name, parameters: Vec<Parameter>, value: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Function(Function {
                name,
                parameters,
                value,
            })
            .into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub patt: Node,
    pub value: Option<Node>,
    pub span: Span,
}

impl Parameter {
    pub fn new(patt: Node, value: Option<Node>, span: Span) -> Parameter {
        Parameter { patt, value, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Def {
    pub lvalue: Node,
    pub rvalue: Node,
}

impl Node {
    pub fn def(lvalue: Node, rvalue: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Def(Def { lvalue, rvalue }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub nodes: Vec<Node>,
}

impl Node {
    pub fn block(nodes: Vec<Node>, span: Span) -> Node {
        Node {
            kind: NodeKind::Block(Block { nodes }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Get {
    pub node: Node,
    pub field: Node,
}

impl Node {
    pub fn get(node: Node, field: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Get(Get { node, field }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Argument {
    pub value: Node,
    pub name: Option<Name>,
    pub span: Span,
}

impl Argument {
    pub fn new(value: Node, name: Option<Name>, span: Span) -> Argument {
        Argument { value, name, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Apply {
    pub callee: Node,
    pub arguments: Vec<Argument>,
}

impl Node {
    pub fn apply(callee: Node, arguments: Vec<Argument>, span: Span) -> Node {
        Node {
            kind: NodeKind::Apply(Apply { callee, arguments }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
}

impl Node {
    pub fn unary(operator: Operator, expr: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Unary(Unary { operator, expr }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Node,
    pub rexpr: Node,
}

impl Node {
    pub fn binary(operator: Operator, lexpr: Node, rexpr: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Binary(Binary {
                operator,
                lexpr,
                rexpr,
            })
            .into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Set {
    pub operator: Operator,
    pub node: Node,
    pub value: Node,
}

impl Node {
    pub fn set(operator: Operator, node: Node, value: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Set(Set {
                operator,
                node,
                value,
            })
            .into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Group {
    pub inner: Node,
}

impl Node {
    pub fn group(inner: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Group(Group { inner }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub items: Vec<Node>,
}

impl Node {
    pub fn tuple(items: Vec<Node>, span: Span) -> Node {
        Node {
            kind: NodeKind::Tuple(Tuple { items }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct List {
    pub items: Vec<Node>,
}

impl Node {
    pub fn list(items: Vec<Node>, span: Span) -> Node {
        Node {
            kind: NodeKind::List(List { items }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

impl Node {
    pub fn literal(kind: fn(Literal) -> NodeKind, value: String, span: Span) -> Node {
        Node {
            kind: kind(Literal { value, span }).into(),
            span,
        }
    }
}

pub type Name = Literal;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub module: Name,
    pub names: Vec<Name>,
}

impl Node {
    pub fn import(module: Name, names: Vec<Name>, span: Span) -> Node {
        Node {
            kind: NodeKind::Import(Import { module, names }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorKind {
    Is,    // is
    In,    // in
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

    // Assignment
    Sets, // :=
    Adds, // +
    Subs, // -
    Muls, // *
    Divs, // /
    Mods, // %
}

pub type Precedence = i8;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    None,
    Left,
    Right,
}

impl Operator {
    pub fn new(kind: OperatorKind, span: Span) -> Operator {
        Operator { kind, span }
    }

    pub fn is_unary(self) -> bool {
        matches!(
            self.kind,
            OperatorKind::Pos | OperatorKind::Neg | OperatorKind::Not
        )
    }

    pub fn precedence(self) -> Precedence {
        match self.kind {
            OperatorKind::Pos
          | OperatorKind::Neg
          | OperatorKind::Not => -1,
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
            OperatorKind::Is | OperatorKind::In => 4,
            OperatorKind::And => 3,
            OperatorKind::Or => 2,
            OperatorKind::Range => 1,
            OperatorKind::Sets
          | OperatorKind::Adds
          | OperatorKind::Subs
          | OperatorKind::Muls
          | OperatorKind::Divs
          | OperatorKind::Mods => 0,
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

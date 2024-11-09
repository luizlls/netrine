use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Identifier(Literal),
    Underscore(Literal),
    Function(Box<Function>),
    Def(Box<Def>),
    Get(Box<Get>),
    Apply(Box<Apply>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Group(Box<Group>),
    Block(Block),
    Array(Array),
    Tuple(Tuple),
    String(Literal),
    Number(Literal),
    Integer(Literal),
}

impl Node {
    pub fn function_like(&self) -> bool {
        matches!(
            self.kind,
            NodeKind::Apply(box Apply {
                callable: Node {
                    kind: NodeKind::Identifier(_), ..
                },
                ..
            })
        )
    }

    pub fn pattern_like(&self) -> bool {
        matches!(
            self.kind,
            NodeKind::Identifier(_)
          | NodeKind::Underscore(_)
          | NodeKind::Array(_) | NodeKind::Tuple(_)
          | NodeKind::Group(_) | NodeKind::String(_)
          | NodeKind::Number(_) | NodeKind::Integer(_)
          | NodeKind::Unary(box Unary {
                operator: Operator {
                    kind: OperatorKind::Neg | OperatorKind::Pos, ..
                },
                ..
            }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]    
pub struct Name {
    pub value: String,
    pub span: Span,
}

impl Name {
    pub fn new(value: String, span: Span) -> Name {
        Name {
            value, span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Name,
    pub parameters: Vec<Node>,
    pub value: Node,
}

impl Node {
    pub fn function(name: Name, parameters: Vec<Node>, value: Node) -> Node {
        let span = name.span.to(value.span);
        Node {
            kind: NodeKind::Function(Function { name, parameters, value }.into()),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Def {
    pub lvalue: Node,
    pub rvalue: Node,
}

impl Node {
    pub fn define(lvalue: Node, rvalue: Node) -> Node {
        let span = lvalue.span.to(rvalue.span);
        Node {
            kind: NodeKind::Def(Def { lvalue, rvalue }.into()),
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
    pub fn get(node: Node, field: Node) -> Node {
        let span = node.span.to(field.span);
        Node {
            kind: NodeKind::Get(Get { node, field }.into()),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Apply {
    pub callable: Node,
    pub arguments: Vec<Node>,
}

impl Node {
    pub fn apply(callable: Node, arguments: Vec<Node>, span: Span) -> Node {
        Node {
            kind: NodeKind::Apply(Apply { callable, arguments }.into()),
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
    pub fn unary(operator: Operator, expr: Node) -> Node {
        let span = operator.span.to(expr.span);
        Node {
            kind: NodeKind::Unary(Unary { operator, expr }.into()),
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
    pub fn binary(operator: Operator, lexpr: Node, rexpr: Node) -> Node {
        let span = lexpr.span.to(rexpr.span);
        Node {
            kind: NodeKind::Binary(Binary { operator, lexpr, rexpr }.into()),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Group {
    pub inner: Node,
}

impl Node {
    pub fn group(mut items: Vec<Node>, span: Span) -> Node {
        let inner = items.pop().unwrap();
        Node {
            kind: NodeKind::Group(Group { inner }.into()),
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
            kind: NodeKind::Block(Block { nodes }),
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
            kind: NodeKind::Tuple(Tuple { items }),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
    pub items: Vec<Node>,
}

impl Node {
    pub fn array(items: Vec<Node>, span: Span) -> Node {
        Node {
            kind: NodeKind::Array(Array { items }),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub value: String,
}

impl Node {
    pub fn literal(value: String, span: Span, ctor: fn(Literal) -> NodeKind) -> Node {
        Node {
            kind: ctor(Literal { value }),
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
        matches!(self.kind, OperatorKind::Pos | OperatorKind::Neg | OperatorKind::Not)
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

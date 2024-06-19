use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Fn(Box<Fn>),
    Def(Box<Def>),
    Get(Box<Get>),
    Apply(Box<Apply>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Group(Box<Group>),
    Block(Block),
    Array(Array),
    Tuple(Tuple),
    Identifier(Ident),
    Underscore(Ident),
    String(Literal),
    Number(Literal),
    Integer(Literal),
}

impl Node {
    pub fn span(&self) -> Span {
        match self {
            Node::Fn(n) => n.span,
            Node::Def(n) => n.span,
            Node::Get(n) => n.span,
            Node::Apply(n) => n.span,
            Node::Unary(n) => n.span,
            Node::Binary(n) => n.span,
            Node::Block(n) => n.span,
            Node::Group(n) => n.span,
            Node::Array(n) => n.span,
            Node::Tuple(n) => n.span,
            Node::Identifier(n) => n.span,
            Node::Underscore(n) => n.span,
            Node::String(n) => n.span,
            Node::Number(n) => n.span,
            Node::Integer(n) => n.span,
        }
    }

    pub fn function_like(&self) -> bool {
        matches!(
            self,
            Node::Apply(box Apply {
                callable: Node::Identifier(_),
                ..
            })
        )
    }

    pub fn pattern_like(&self) -> bool {
        matches!(
            self,
            Node::Identifier(_)
          | Node::Underscore(_)
          | Node::Array(_) | Node::Tuple(_)
          | Node::Group(_) | Node::String(_)
          | Node::Number(_) | Node::Integer(_)
          | Node::Unary(box Unary {
                operator: Operator {
                    kind: OperatorKind::Neg | OperatorKind::Pos, ..
                },
                ..
            }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fn {
    pub name: Ident,
    pub parameters: Vec<Node>,
    pub value: Node,
    pub span: Span,
}

impl Node {
    pub fn function(name: Ident, parameters: Vec<Node>, value: Node) -> Node {
        let span = name.span.to(value.span());
        Node::Fn(Fn { name, parameters, value, span }.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Def {
    pub lvalue: Node,
    pub rvalue: Node,
    pub span: Span,
}

impl Node {
    pub fn define(lvalue: Node, rvalue: Node) -> Node {
        let span = lvalue.span().to(rvalue.span());
        Node::Def(Def { lvalue, rvalue, span }.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Get {
    pub node: Node,
    pub field: Node,
    pub span: Span,
}

impl Node {
    pub fn get(node: Node, field: Node) -> Node {
        let span = node.span().to(field.span());
        Node::Get(Get { node, field, span }.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Apply {
    pub callable: Node,
    pub arguments: Vec<Node>,
    pub span: Span,
}

impl Node {
    pub fn apply(callable: Node, arguments: Vec<Node>, span: Span) -> Node {
        Node::Apply(Apply { callable, arguments, span }.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
    pub span: Span,
}

impl Node {
    pub fn unary(operator: Operator, expr: Node) -> Node {
        let span = operator.span.to(expr.span());
        Node::Unary(Unary { operator, expr, span }.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Node,
    pub rexpr: Node,
    pub span: Span,
}

impl Node {
    pub fn binary(operator: Operator, lexpr: Node, rexpr: Node) -> Node {
        let span = lexpr.span().to(rexpr.span());
        Node::Binary(Binary { operator, lexpr, rexpr, span }.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Group {
    pub inner: Node,
    pub span: Span,
}

impl Node {
    pub fn group(mut items: Vec<Node>, span: Span) -> Node {
        let inner = items.pop().unwrap();
        Node::Group(Group { inner, span }.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub nodes: Vec<Node>,
    pub span: Span,
}

impl Node {
    pub fn block(nodes: Vec<Node>, span: Span) -> Node {
        Node::Block(Block { nodes, span })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub items: Vec<Node>,
    pub span: Span,
}

impl Node {
    pub fn tuple(items: Vec<Node>, span: Span) -> Node {
        Node::Tuple(Tuple { items, span })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
    pub items: Vec<Node>,
    pub span: Span,
}

impl Node {
    pub fn array(items: Vec<Node>, span: Span) -> Node {
        Node::Array(Array { items, span })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

pub type Ident = Literal;

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

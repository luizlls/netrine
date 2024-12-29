use std::fmt::{self, Display, Formatter};

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Group(Box<Group>),
    Number(Literal),
    Integer(Literal),
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
    pub fn group(inner: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Group(Group { inner }.into()),
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

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let description = match self.kind {
            OperatorKind::Is => "is",
            OperatorKind::In => "in",
            OperatorKind::And => "and",
            OperatorKind::Or => "or",
            OperatorKind::Not => "not",
            OperatorKind::Pos => "pos",
            OperatorKind::Neg => "neg",
            OperatorKind::Add => "add",
            OperatorKind::Sub => "sub",
            OperatorKind::Mul => "mul",
            OperatorKind::Div => "div",
            OperatorKind::Mod => "mod",
            OperatorKind::Exp => "exp",
            OperatorKind::Eq => "eq",
            OperatorKind::Ne => "ne",
            OperatorKind::Lt => "lt",
            OperatorKind::Le => "le",
            OperatorKind::Gt => "gt",
            OperatorKind::Ge => "ge",
            OperatorKind::Range => "range",
        };

        write!(f, "{description}")
    }
}

impl Node {

    pub fn dump(&self) -> String {
        match &self.kind {
            NodeKind::Unary(unary) => {
                format!("({} {})", unary.operator, unary.expr.dump())
            }
            NodeKind::Binary(binary) => {
                format!("({} {} {})", binary.operator, binary.lexpr.dump(), binary.rexpr.dump())
            }
            NodeKind::Group(group) => {
                group.inner.dump()
            }
            NodeKind::Number(literal)
          | NodeKind::Integer(literal) => literal.value.clone(),
        }
    }
}

use std::fmt::{self, Display, Formatter};

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node<T> {
    pub kind: NodeKind<T>,
    pub data: T,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind<T> {
    Unary(Unary<T>),
    Binary(Binary<T>),
    Group(Group<T>),
    Number(Literal),
    Integer(Literal),
}

impl<T> Display for Node<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            NodeKind::Unary(unary) => write!(f, "{}", unary),
            NodeKind::Binary(binary) => write!(f, "{}", binary),
            NodeKind::Group(group) => write!(f, "({})", group),
            NodeKind::Number(literal)
          | NodeKind::Integer(literal) => write!(f, "{}", literal),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unary<T> {
    pub op: Operator,
    pub expr: Box<Node<T>>,
}

impl<T> Node<T> {
    pub fn unary(op: Operator, expr: Node<T>, data: T) -> Node<T> {
        Node {
            kind: NodeKind::Unary(Unary {
                op,
                expr: Box::new(expr),
            }),
            data,
        }
    }
}

impl<T> Display for Unary<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({} {})", self.op, self.expr)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary<T> {
    pub op: Operator,
    pub lexpr: Box<Node<T>>,
    pub rexpr: Box<Node<T>>,
}

impl<T> Node<T> {
    pub fn binary(op: Operator, lexpr: Node<T>, rexpr: Node<T>, data: T) -> Node<T> {
        Node {
            kind: NodeKind::Binary(Binary {
                op,
                lexpr: Box::new(lexpr),
                rexpr: Box::new(rexpr),
            }),
            data,
        }
    }
}

impl<T> Display for Binary<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.op, self.lexpr, self.rexpr)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Group<T> {
    pub inner: Box<Node<T>>,
}

impl<T> Node<T> {
    pub fn group(inner: Node<T>, data: T) -> Node<T> {
        Node {
            kind: NodeKind::Group(Group {
                inner: Box::new(inner),
            }),
            data,
        }
    }
}

impl<T> Display for Group<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub value: String,
}

impl<T> Node<T> {
    pub fn literal(value: String, ctor: fn(Literal) -> NodeKind<T>, data: T) -> Node<T> {
        Node {
            kind: ctor(Literal { value }),
            data,
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
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
            OperatorKind::Is
          | OperatorKind::In => 4,
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

use std::fmt::{self, Display};

use crate::interner::Name;
use crate::source::Span;
use crate::types::{self, TypeId};

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub type_id: TypeId,
}

impl Node {
    pub fn new(kind: NodeKind, span: Span, type_id: TypeId) -> Node {
        Node {
            kind,
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Function(Box<Function>),
    Definition(Box<Definition>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Apply(Box<Apply>),
    Reference(Reference),
    Integer(Integer),
    Number(Number),
    True,
    False,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Name,
    pub parameters: Vec<Parameter>,
    pub value: Node,
}

impl Node {
    pub fn function(
        name: Name,
        parameters: Vec<Parameter>,
        value: Node,
        span: Span,
        type_id: TypeId,
    ) -> Node {
        Node {
            kind: NodeKind::Function(
                Function {
                    name,
                    parameters,
                    value,
                }
                .into(),
            ),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Name,
    pub type_id: TypeId,
}

impl Parameter {
    pub fn new(name: Name, type_id: TypeId) -> Parameter {
        Parameter { name, type_id }
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub name: Name,
    pub value: Node,
}

impl Node {
    pub fn definition(name: Name, value: Node, span: Span, type_id: TypeId) -> Node {
        Node {
            kind: NodeKind::Definition(Definition { name, value }.into()),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Reference {
    pub kind: ReferenceKind,
    pub name: Name,
}

#[derive(Debug, Clone)]
pub enum ReferenceKind {
    Local,
    Global,
    Function,
}

impl Node {
    pub fn reference(kind: ReferenceKind, name: Name, span: Span, type_id: TypeId) -> Node {
        Node {
            kind: NodeKind::Reference(Reference { kind, name }),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub callee: Node,
    pub arguments: Vec<Node>,
}

impl Node {
    pub fn apply(callee: Node, arguments: Vec<Node>, span: Span, type_id: TypeId) -> Node {
        Node {
            kind: NodeKind::Apply(
                Apply {
                    callee,
                    arguments,
                }
                .into(),
            ),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub operand: Node,
}

impl Node {
    pub fn unary(operator: Operator, operand: Node, span: Span, type_id: TypeId) -> Node {
        Node {
            kind: NodeKind::Unary(
                Unary {
                    operator,
                    operand,
                }
                .into(),
            ),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub loperand: Node,
    pub roperand: Node,
}

impl Node {
    pub fn binary(
        operator: Operator,
        loperand: Node,
        roperand: Node,
        span: Span,
        type_id: TypeId,
    ) -> Node {
        Node {
            kind: NodeKind::Binary(
                Binary {
                    operator,
                    loperand,
                    roperand,
                }
                .into(),
            ),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Node {
    pub fn integer(value: i64, span: Span) -> Node {
        Node {
            kind: NodeKind::Integer(Integer { value }),
            span,
            type_id: types::INTEGER,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Number {
    pub value: f64,
}

impl Node {
    pub fn number(value: f64, span: Span) -> Node {
        Node {
            kind: NodeKind::Number(Number { value }),
            span,
            type_id: types::NUMBER,
        }
    }
}

impl Node {
    pub fn bool(truthy: bool, span: Span) -> Node {
        Node {
            kind: if truthy {
                NodeKind::True
            } else {
                NodeKind::False
            },
            span,
            type_id: types::BOOLEAN,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    And,
    Or,
    Not,
    Pos,
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let description = match self {
            Operator::Pos => "pos",
            Operator::Neg => "neg",
            Operator::Add => "add",
            Operator::Sub => "sub",
            Operator::Mul => "mul",
            Operator::Div => "div",
            Operator::Mod => "mod",
            Operator::Pow => "pow",
            Operator::Eq => "eq",
            Operator::Ne => "ne",
            Operator::Lt => "lt",
            Operator::Le => "le",
            Operator::Gt => "gt",
            Operator::Ge => "ge",
            Operator::And => "and",
            Operator::Or => "or",
            Operator::Not => "not",
        };

        write!(f, "{description}")
    }
}

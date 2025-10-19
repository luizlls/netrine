use std::fmt::{self, Display};

use crate::pprint::{PrettyPrint, PrettyPrintNode};
use crate::source::{Span, ToSpan};
use crate::state::{NameId, State};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct NodeId(pub(crate) u32);

impl NodeId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) nodes: Vec<Node>,
}

impl PrettyPrint for Module {
    fn print(&self, _state: &State) -> PrettyPrintNode<'_> {
        let mut printer = PrettyPrintNode::printer();
        for node in &self.nodes {
            printer.add_child(node);
        }
        printer.print()
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub(crate) id: NodeId,
    pub(crate) kind: NodeKind,
    pub(crate) span: Span,
}

impl ToSpan for Node {
    fn span(&self) -> Span {
        self.span
    }
}

impl PrettyPrint for Node {
    fn print(&self, state: &State) -> PrettyPrintNode<'_> {
        match &self.kind {
            NodeKind::Define(define) => {
                PrettyPrintNode::printer()
                    .label(format!("DEFINE {}", self.span))
                    .child(&define.name)
                    .child(&define.value)
                    .print()
            }
            NodeKind::Unary(unary) => {
                PrettyPrintNode::printer()
                    .label(format!("UNARY {}", self.span))
                    .child(&unary.operator)
                    .child(&unary.expr)
                    .print()
            }
            NodeKind::Binary(binary) => {
                PrettyPrintNode::printer()
                    .label(format!("BINARY {}", self.span))
                    .child(&binary.lexpr)
                    .child(&binary.operator)
                    .child(&binary.rexpr)
                    .print()
            }
            NodeKind::Name(name) => name.print(state),
            NodeKind::Number(number) => {
                PrettyPrintNode::printer()
                    .label(format!("NUMBER({}) {}", number.value, self.span))
                    .print()
            }
            NodeKind::Integer(integer) => {
                PrettyPrintNode::printer()
                    .label(format!("INTEGER({}) {}", integer.value, self.span))
                    .print()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Define(Box<Define>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Name(Name),
    Number(Literal),
    Integer(Literal),
}

#[derive(Debug, Clone)]
pub struct Define {
    pub(crate) name: Name,
    pub(crate) value: Node,
}

impl From<Define> for NodeKind {
    fn from(define: Define) -> NodeKind {
        NodeKind::Define(Box::new(define))
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub(crate) operator: Operator,
    pub(crate) expr: Node,
}

impl From<Unary> for NodeKind {
    fn from(unary: Unary) -> NodeKind {
        NodeKind::Unary(Box::new(unary))
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub(crate) operator: Operator,
    pub(crate) lexpr: Node,
    pub(crate) rexpr: Node,
}

impl From<Binary> for NodeKind {
    fn from(binary: Binary) -> NodeKind {
        NodeKind::Binary(Box::new(binary))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Name {
    pub(crate) name: NameId,
    pub(crate) span: Span,
}

impl ToSpan for Name {
    fn span(&self) -> Span {
        self.span
    }
}

impl PrettyPrint for Name {
    fn print(&self, state: &State) -> PrettyPrintNode<'_> {
        PrettyPrintNode::printer()
            .label(format!("NAME({}) {}", state.interner.get(self.name).unwrap(), self.span))
            .print()
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub(crate) value: Box<str>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Operator {
    pub(crate) kind: OperatorKind,
    pub(crate) span: Span,
}

impl PrettyPrint for Operator {
    fn print(&self, _state: &State) -> PrettyPrintNode<'_> {
        PrettyPrintNode::printer()
            .label(format!("OPERATOR({}) {}", self, self.span))
            .print()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OperatorKind {
    And, // and
    Or,  // or
    Not, // not
    Pos, // +
    Neg, // -
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Pow, // ^
    Eq,  // ==
    Ne,  // !=
    Lt,  // <
    Le,  // <=
    Gt,  // >
    Ge,  // >=
}

pub(crate) type Precedence = u8;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    None,
    Left,
    Right,
}

impl Operator {
    #[rustfmt::skip]
    pub fn precedence(self) -> Precedence {
        match self.kind {
            OperatorKind::Pos
          | OperatorKind::Neg
          | OperatorKind::Not => 0,
            OperatorKind::Pow => 7,
            OperatorKind::Mul
          | OperatorKind::Div => 6,
            OperatorKind::Add
          | OperatorKind::Sub => 5,
            OperatorKind::Mod => 4,
            OperatorKind::Lt
          | OperatorKind::Le
          | OperatorKind::Gt
          | OperatorKind::Ge
          | OperatorKind::Ne
          | OperatorKind::Eq => 3,
            OperatorKind::And => 2,
            OperatorKind::Or => 1,
        }
    }

    #[rustfmt::skip]
    pub fn associativity(self) -> Associativity {
        match self.kind {
            OperatorKind::Pos
          | OperatorKind::Neg
          | OperatorKind::Not => Associativity::None,
            OperatorKind::Pow => Associativity::Right,
            _ => Associativity::Left,
        }
    }

    pub fn next_precedence(self) -> Precedence {
        if self.associativity() == Associativity::Right {
            self.precedence()
        } else {
            self.precedence() + 1
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let description = match self.kind {
            OperatorKind::Pos => "+",
            OperatorKind::Neg => "-",
            OperatorKind::Add => "+",
            OperatorKind::Sub => "-",
            OperatorKind::Mul => "*",
            OperatorKind::Div => "/",
            OperatorKind::Mod => "%",
            OperatorKind::Pow => "^",
            OperatorKind::Eq => "==",
            OperatorKind::Ne => "!=",
            OperatorKind::Lt => "<",
            OperatorKind::Le => "<=",
            OperatorKind::Gt => ">",
            OperatorKind::Ge => ">=",
            OperatorKind::And => "and",
            OperatorKind::Or => "or",
            OperatorKind::Not => "not",
        };

        write!(f, "{description}")
    }
}

impl ToSpan for Operator {
    fn span(&self) -> Span {
        self.span
    }
}

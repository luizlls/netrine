use std::fmt::{self, Display};

use crate::source::{Span, ToSpan};

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Node>,
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for node in &self.nodes {
            writeln!(f, "{node}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub(crate) kind: NodeKind,
    pub(crate) span: Span,
}

impl ToSpan for Node {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for Node {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            NodeKind::Define(define) => write!(f, "{define}"),
            NodeKind::Unary(unary) => write!(f, "{unary}"),
            NodeKind::Binary(binary) => write!(f, "{binary}"),
            NodeKind::Name(name) => write!(f, "{name}"),
            NodeKind::Number(literal)
          | NodeKind::Integer(literal) => write!(f, "{literal}"),
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

impl Display for Define {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(let {} {})", self.name, self.value)
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

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.operator, self.expr)
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

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.operator, self.lexpr, self.rexpr)
    }
}

#[derive(Debug, Clone)]
pub struct Name {
    pub(crate) value: String,
    pub(crate) span: Span,
}

impl ToSpan for Name {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub(crate) value: String,
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Operator {
    pub(crate) kind: OperatorKind,
    pub(crate) span: Span,
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

use std::fmt::{self, Display};

use crate::pprint::{PrettyPrint, PrettyPrintNode, PrettyPrinter};
use crate::source::{Span, ToSpan};

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Node>,
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for node in &self.nodes {
            write!(f, "{node}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Number(Literal),
    Integer(Literal),
}

impl ToSpan for Node {
    #[rustfmt::skip]
    fn span(&self) -> Span {
        match self {
            Node::Unary(unary) => unary.span,
            Node::Binary(binary) => binary.span,
            Node::Number(literal)
          | Node::Integer(literal) => literal.span,
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pprint(f)
    }
}

impl PrettyPrint for Node {
    fn print(&self) -> PrettyPrintNode<'_> {
        match self {
            Node::Unary(unary) => unary.print(),
            Node::Binary(binary) => binary.print(),
            Node::Number(number) => {
                PrettyPrintNode::printer()
                    .label(format!("NUMBER({}) {}", number.value, number.span))
                    .print()
            }
            Node::Integer(integer) => {
                PrettyPrintNode::printer()
                    .label(format!("INTEGER({}) {}", integer.value, integer.span))
                    .print()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
    pub span: Span,
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pprint(f)
    }
}

impl PrettyPrint for Unary {
    fn print(&self) -> PrettyPrintNode<'_> {
        PrettyPrintNode::printer()
            .label(format!("UNARY {}", self.span))
            .child(&self.operator)
            .child(&self.expr)
            .print()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Node,
    pub rexpr: Node,
    pub span: Span,
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pprint(f)
    }
}

impl PrettyPrint for Binary {
    fn print(&self) -> PrettyPrintNode<'_> {
        PrettyPrintNode::printer()
            .label(format!("BINARY {}", self.span))
            .child(&self.lexpr)
            .child(&self.operator)
            .child(&self.rexpr)
            .print()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub value: Box<str>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

impl PrettyPrint for Operator {
    fn print(&self) -> PrettyPrintNode<'_> {
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
    Exp, // ^
    Eq,  // ==
    Ne,  // !=
    Lt,  // <
    Le,  // <=
    Gt,  // >
    Ge,  // >=
}

pub type Precedence = u8;

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
            OperatorKind::Exp => 7,
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
            OperatorKind::Exp => Associativity::Right,
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
            OperatorKind::Exp => "^",
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

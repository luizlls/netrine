use std::fmt::{self, Display, Formatter};

use crate::source::{Span, ToSpan};

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Binary(Operator, Box<Node>, Box<Node>),
    Unary(Operator, Box<Node>),
    Group(Box<Node>),
    Number(Literal),
    Integer(Literal),
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        fn write(f: &mut Formatter, node: &Node, depth: usize) -> fmt::Result {

            write!(f, "{}", " ".repeat(depth))?;

            match &node.kind {
                NodeKind::Unary(operator, expr) => {
                    writeln!(f, "UNARY ({}) {}", format!("{}", operator).to_uppercase(), node.span)?;
                    write(f, expr, depth + 2)?;
                }
                NodeKind::Binary(operator, lexpr, rexpr) => {
                    writeln!(f, "BINARY ({}) {}", format!("{}", operator).to_uppercase(), node.span)?;
                    write(f, lexpr, depth + 2)?;
                    write(f, rexpr, depth + 2)?;
                }
                NodeKind::Group(inner) => {
                    writeln!(f, "GROUP {}", node.span)?;
                    write(f, &inner, depth + 2)?;
                }
                NodeKind::Number(literal) => {
                    writeln!(f, "NUMBER ({}) {}", literal.value, literal.span)?;
                }
                NodeKind::Integer(literal) => {
                    writeln!(f, "INTEGER ({}) {}", literal.value, literal.span)?;
              }
            }

            Ok(())
        }

        write(f, self, 0)
    }
}

impl ToSpan for Node {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
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

impl ToSpan for Operator {
    fn span(&self) -> Span {
        self.span
    }
}

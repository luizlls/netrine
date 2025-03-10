use std::fmt::{self, Display, Formatter};

use crate::source::{Span, ToSpan};

#[derive(Debug, Clone)]
pub enum Node {
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Group(Box<Group>),
    Number(Literal),
    Integer(Literal),
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        fn write(f: &mut Formatter, node: &Node, depth: usize) -> fmt::Result {

            write!(f, "{}", " ".repeat(depth))?;

            match &node {
                Node::Unary(unary) => {
                    writeln!(f, "UNARY ({}) {}", format!("{}", unary.operator).to_uppercase(), unary.span)?;
                    write(f, &unary.expr, depth + 2)?;
                }
                Node::Binary(binary) => {
                    writeln!(f, "BINARY ({}) {}", format!("{}", binary.operator).to_uppercase(), binary.span)?;
                    write(f, &binary.lexpr, depth + 2)?;
                    write(f, &binary.rexpr, depth + 2)?;
                }
                Node::Group(group) => {
                    writeln!(f, "GROUP {}", group.span)?;
                    write(f, &group.inner, depth + 2)?;
                }
                Node::Number(literal) => {
                    writeln!(f, "NUMBER ({}) {}", literal.value, literal.span)?;
                }
                Node::Integer(literal) => {
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
        match self {
            Node::Unary(unary) => unary.span,
            Node::Binary(binary) => binary.span,
            Node::Group(group) => group.span,
            Node::Number(literal) => literal.span,
            Node::Integer(literal) => literal.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Node,
    pub rexpr: Node,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Group {
    pub inner: Node,
    pub span: Span,
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

impl Operator {
    pub fn new(kind: OperatorKind, span: Span) -> Operator {
        Operator {
            kind,
            span,
        }
    }
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

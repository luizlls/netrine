use std::fmt::{self, Display, Formatter};

use crate::source::Span;

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

impl Node {
    pub fn span(&self) -> Span {
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
pub enum Operator {
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
    pub fn is_unary(self) -> bool {
        matches!(self, Operator::Pos | Operator::Neg | Operator::Not)
    }

    pub fn precedence(self) -> Precedence {
        match self {
            Operator::Pos
          | Operator::Neg
          | Operator::Not => -1,
            Operator::Exp => 9,
            Operator::Mul
          | Operator::Div => 8,
            Operator::Add
          | Operator::Sub => 7,
            Operator::Mod => 6,
            Operator::Lt
          | Operator::Le
          | Operator::Gt
          | Operator::Ge
          | Operator::Ne
          | Operator::Eq => 5,
            Operator::Is
          | Operator::In => 4,
            Operator::And => 3,
            Operator::Or => 2,
            Operator::Range => 1,
        }
    }

    pub fn associativity(self) -> Associativity {
        match self {
            Operator::Pos
          | Operator::Neg
          | Operator::Not => Associativity::None,
            Operator::Exp => Associativity::Right,
            _ => Associativity::Left,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let description = match self {
            Operator::Is => "is",
            Operator::In => "in",
            Operator::And => "and",
            Operator::Or => "or",
            Operator::Not => "not",
            Operator::Pos => "pos",
            Operator::Neg => "neg",
            Operator::Add => "add",
            Operator::Sub => "sub",
            Operator::Mul => "mul",
            Operator::Div => "div",
            Operator::Mod => "mod",
            Operator::Exp => "exp",
            Operator::Eq => "eq",
            Operator::Ne => "ne",
            Operator::Lt => "lt",
            Operator::Le => "le",
            Operator::Gt => "gt",
            Operator::Ge => "ge",
            Operator::Range => "range",
        };

        write!(f, "{description}")
    }
}

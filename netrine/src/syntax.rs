use std::fmt::{self, Display};

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

#[derive(Debug, Clone)]
pub enum Node {
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Number(Literal),
    Integer(Literal),
}

impl ToSpan for Node {
    fn span(&self) -> Span {
        match self {
            Node::Unary(unary) => unary.span,
            Node::Binary(binary) => binary.span,
            Node::Number(literal)
          | Node::Integer(literal) => literal.span,
        }
    }
}

struct DisplayNode(String, Span, Vec<DisplayNode>);

impl DisplayNode {
    fn write(self, f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result {
        let DisplayNode(value, span, children) = self;
        writeln!(f, "{}{} {}", "  ".repeat(depth), value, span)?;
        for child in children {
            child.write(f, depth + 1)?;
        }
        Ok(())
    }
}

impl Node {
    fn display(&self) -> DisplayNode {
        match self {
            Node::Unary(unary) => {
                DisplayNode("UNARY".to_string(), unary.span, vec![
                    DisplayNode(format!("OPERATOR `{}`", unary.operator), unary.operator.span, vec![]),
                    unary.expr.display(),
                ])
            },
            Node::Binary(binary) => {
                DisplayNode("BINARY".to_string(), binary.span, vec![
                    binary.lexpr.display(),
                    DisplayNode(format!("OPERATOR `{}`", binary.operator), binary.operator.span, vec![]),
                    binary.rexpr.display(),
                ])
            },
            Node::Number(literal) => {
                DisplayNode(format!("NUMBER `{}`", literal.value), literal.span, vec![])
            }
            Node::Integer(literal) => {
                DisplayNode(format!("INTEGER `{}`", literal.value), literal.span, vec![])
            },
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display().write(f, 0)
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

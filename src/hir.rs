use std::fmt::{self, Display, Formatter};

use crate::ast::Operator;
use crate::source::Span;
use crate::types::TypeId;

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Unary(Unary),
    Binary(Binary),
    Number(f64),
    Integer(i64),
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            NodeKind::Unary(unary) => write!(f, "{unary}"),
            NodeKind::Binary(binary) => write!(f, "{binary}"),
            NodeKind::Number(number) => write!(f, "{number}"),
          | NodeKind::Integer(integer) => write!(f, "{integer}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Box<Node>,
}

impl Display for Unary {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({} {})", self.operator, self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Box<Node>,
    pub rexpr: Box<Node>,
}

impl Display for Binary {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.operator, self.lexpr, self.rexpr)
    }
}

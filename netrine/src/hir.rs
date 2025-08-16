use std::fmt::{self, Display};

use crate::error::{Error, Result};
use crate::source::Span;
use crate::syntax;
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub enum Node {
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Number(Number),
    Integer(Integer),
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
    pub span: Span,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Node,
    pub rexpr: Node,
    pub span: Span,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Number {
    pub value: f64,
    pub span: Span,
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
    Exp,
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
            Operator::Exp => "exp",
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


struct LowerSyntax {
}

impl LowerSyntax {
    fn node(&mut self, node: &syntax::Node) -> Result<Node> {
        match node {
            syntax::Node::Binary(node) => self.binary(node),
            syntax::Node::Unary(node) => self.unary(node),
            syntax::Node::Number(literal) => self.number(literal),
            syntax::Node::Integer(literal) => self.integer(literal),
        }
    }

    fn binary(&mut self, binary: &syntax::Binary) -> Result<Node> {
        let operator = self.operator(binary.operator);

        let type_ = match operator {
            Operator::Add
          | Operator::Sub
          | Operator::Mul
          | Operator::Div
          | Operator::Exp
          | Operator::Mod => Type::Number,
            Operator::Eq
          | Operator::Ne
          | Operator::Lt
          | Operator::Le
          | Operator::Gt
          | Operator::Ge
          | Operator::And
          | Operator::Or => Type::Boolean,
            _ => unreachable!("Binary instruction with unsupported operator {}", operator)
        };

        let lexpr = self.node(&binary.lexpr)?;
        let rexpr = self.node(&binary.rexpr)?;

        Ok(Node::Binary(
            Binary {
                operator,
                lexpr,
                rexpr,
                type_,
                span: binary.span,
            }.into()
        ))
    }

    fn unary(&mut self, unary: &syntax::Unary) -> Result<Node> {
        let operator = self.operator(unary.operator);

        let type_ = match operator {
            Operator::Not => Type::Boolean,
            Operator::Pos
          | Operator::Neg => Type::Number,
            _ => unreachable!("Unary instruction with unsupported operator {}", operator)
        };

        let expr = self.node(&unary.expr)?;

        Ok(Node::Unary(
            Unary {
                operator,
                expr,
                type_,
                span: unary.span,
            }.into()
        ))
    }

    fn number(&mut self, number: &syntax::Literal) -> Result<Node> {
        let Ok(value) = str::parse(&number.value) else {
            return Err(Error::new(
                "value is not supported as a number".to_string(),
                number.span,
            ));
        };

        Ok(Node::Number(
            Number {
                value,
                span: number.span,
            }
        ))
    }

    fn integer(&mut self, integer: &syntax::Literal) -> Result<Node> {
        let value = match &integer.value.get(0..2) {
            Some("0b") => i64::from_str_radix(&integer.value[2..], 2),
            Some("0x") => i64::from_str_radix(&integer.value[2..], 16),
            _ => str::parse(&integer.value),
        };

        let Ok(value) = value else {
            return Err(Error::new(
                "value is not supported as an integer".to_string(),
                integer.span,
            ));
        };

        Ok(Node::Integer(
            Integer {
                value,
                span: integer.span,
            }
        ))
    }

    fn operator(&self, operator: syntax::Operator) -> Operator {
        match operator.kind {
            syntax::OperatorKind::Pos => Operator::Pos,
            syntax::OperatorKind::Neg => Operator::Neg,
            syntax::OperatorKind::Add => Operator::Add,
            syntax::OperatorKind::Sub => Operator::Sub,
            syntax::OperatorKind::Mul => Operator::Mul,
            syntax::OperatorKind::Div => Operator::Div,
            syntax::OperatorKind::Mod => Operator::Mod,
            syntax::OperatorKind::Exp => Operator::Exp,
            syntax::OperatorKind::Eq => Operator::Eq,
            syntax::OperatorKind::Ne => Operator::Ne,
            syntax::OperatorKind::Lt => Operator::Lt,
            syntax::OperatorKind::Le => Operator::Le,
            syntax::OperatorKind::Gt => Operator::Gt,
            syntax::OperatorKind::Ge => Operator::Ge,
            syntax::OperatorKind::And => Operator::And,
            syntax::OperatorKind::Or => Operator::Or,
            syntax::OperatorKind::Not => Operator::Not,
        }
    }
}

pub fn from_syntax(module: &syntax::Module) -> Result<Module> {
    let mut lower = LowerSyntax {};
    let mut nodes = vec![];

    for node in &module.nodes {
        nodes.push(lower.node(node)?);
    }

    Ok(Module { nodes })
}

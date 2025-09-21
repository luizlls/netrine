use std::fmt::{self, Display};

use crate::error::{Error, Result};
use crate::source::Span;
use crate::syntax;
use crate::types::{self, Type};

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
    Number(Number),
    Integer(Integer),
}

impl Node {
    pub fn span(&self) -> Span {
        match self {
            Node::Unary(unary) => unary.span,
            Node::Binary(binary) => binary.span,
            Node::Number(number) => number.span,
            Node::Integer(integer) => integer.span,
        }
    }
}

struct DisplayNode(String, Type, Vec<DisplayNode>);

impl DisplayNode {
    fn write(self, f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result {
        let DisplayNode(value, type_, children) = self;
        writeln!(f, "{}{}: {}", "  ".repeat(depth), value, type_)?;
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
                DisplayNode("UNARY".to_string(), unary.type_, vec![
                    DisplayNode(format!("OPERATOR `{}`", unary.operator), unary.type_, vec![]),
                    unary.operand.display(),
                ])
            },
            Node::Binary(binary) => {
                DisplayNode("BINARY".to_string(), binary.type_, vec![
                    binary.loperand.display(),
                    DisplayNode(format!("OPERATOR `{}`", binary.operator), binary.type_, vec![]),
                    binary.roperand.display(),
                ])
            },
            Node::Number(literal) => {
                DisplayNode(format!("NUMBER `{}`", literal.value), types::NUMBER, vec![])
            }
            Node::Integer(literal) => {
                DisplayNode(format!("INTEGER `{}`", literal.value), types::INTEGER, vec![])
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
    pub operand: Node,
    pub span: Span,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub loperand: Node,
    pub roperand: Node,
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
    fn new() -> LowerSyntax {
        LowerSyntax {}
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }

    fn node(&mut self, node: &syntax::Node) -> Result<Node> {
        match node {
            syntax::Node::Binary(node) => self.binary(node),
            syntax::Node::Unary(node) => self.unary(node),
            syntax::Node::Number(literal) => self.number(literal),
            syntax::Node::Integer(literal) => self.integer(literal),
        }
    }

    fn binary(&mut self, binary: &syntax::Binary) -> Result<Node> {
        let loperand = self.node(&binary.lexpr)?;
        let roperand = self.node(&binary.rexpr)?;

        let operator = match binary.operator.kind {
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
            _ => {
                return self.fail(binary.operator.span, "unsupported binary operator");
            }
        };

        Ok(Node::Binary(
            Binary {
                operator,
                loperand,
                roperand,
                type_: Type::Unknown,
                span: binary.span,
            }.into()
        ))
    }

    fn unary(&mut self, unary: &syntax::Unary) -> Result<Node> {
        let operand = self.node(&unary.expr)?;

        let operator = match unary.operator.kind {
            syntax::OperatorKind::Pos => Operator::Pos,
            syntax::OperatorKind::Neg => Operator::Neg,
            syntax::OperatorKind::Not => Operator::Not,
            _ => {
                return self.fail(unary.operator.span, "unsupported unary operator");
            }
        };

        Ok(Node::Unary(
            Unary {
                operator,
                operand,
                type_: Type::Unknown,
                span: unary.span,
            }.into()
        ))
    }

    fn number(&mut self, number: &syntax::Literal) -> Result<Node> {
        let Ok(value) = str::parse(&number.value) else {
            return self.fail(number.span, "value is not supported as an number");
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
            return self.fail(integer.span, "value is not supported as an integer");
        };

        Ok(Node::Integer(
            Integer {
                value,
                span: integer.span,
            }
        ))
    }
}

pub fn from_syntax(module: &syntax::Module) -> Result<Module> {
    let mut lower = LowerSyntax::new();
    let mut nodes = vec![];

    for node in &module.nodes {
        nodes.push(lower.node(node)?);
    }

    Ok(Module { nodes })
}

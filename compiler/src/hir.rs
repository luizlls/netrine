use std::fmt::{self, Display};

use crate::error::{Error, Result};
use crate::source::{Span, ToSpan};
use crate::state::{self, State, SymbolId};
use crate::syntax;
use crate::types::{self, Type};

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) nodes: Vec<Node>,
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for node in &self.nodes {
            writeln!(f, "{node}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub(crate) kind: NodeKind,
    pub(crate) span: Span,
    pub(crate) type_: Type,
}

impl ToSpan for Node {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            NodeKind::Define(define) => write!(f, "{define}"),
            NodeKind::Unary(unary) => write!(f, "{unary}"),
            NodeKind::Binary(binary) => write!(f, "{binary}"),
            NodeKind::Local(local) => write!(f, "{local}"),
            NodeKind::Integer(integer) => write!(f, "{integer}"),
            NodeKind::Number(number) => write!(f, "{number}"),
            NodeKind::Boolean(boolean) => write!(f, "{boolean}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Define(Box<Define>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Local(Local),
    Integer(Integer),
    Number(Number),
    Boolean(Boolean),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Define {
    pub(crate) symbol: SymbolId,
    pub(crate) value: Node,
}

impl From<Define> for NodeKind {
    fn from(define: Define) -> NodeKind {
        NodeKind::Define(Box::new(define))
    }
}

impl Display for Define {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(define {} {})", self.symbol, self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub(crate) symbol_id: SymbolId,
}

impl From<Local> for NodeKind {
    fn from(local: Local) -> NodeKind {
        NodeKind::Local(local)
    }
}

impl Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(local {})", self.symbol_id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub(crate) operator: Operator,
    pub(crate) operand: Node,
}

impl From<Unary> for NodeKind {
    fn from(unary: Unary) -> NodeKind {
        NodeKind::Unary(Box::new(unary))
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.operator, self.operand)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub(crate) operator: Operator,
    pub(crate) loperand: Node,
    pub(crate) roperand: Node,
}

impl From<Binary> for NodeKind {
    fn from(binary: Binary) -> NodeKind {
        NodeKind::Binary(Box::new(binary))
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.operator, self.loperand, self.roperand)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Integer {
    pub(crate) value: i64,
}

impl From<Integer> for NodeKind {
    fn from(integer: Integer) -> NodeKind {
        NodeKind::Integer(integer)
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(integer {})", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub(crate) value: f64,
}

impl From<Number> for NodeKind {
    fn from(number: Number) -> NodeKind {
        NodeKind::Number(number)
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(number {})", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Boolean {
    pub(crate) value: bool,
}

impl From<Boolean> for NodeKind {
    fn from(boolean: Boolean) -> NodeKind {
        NodeKind::Boolean(boolean)
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
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
    Pow,
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
            Operator::Pow => "pow",
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

struct LowerSyntax<'hir> {
    state: &'hir mut State,
}

impl<'hir> LowerSyntax<'hir> {
    fn new(state: &'hir mut State) -> LowerSyntax<'hir> {
        LowerSyntax { state }
    }

    fn node(&mut self, span: Span, type_: Type, kind: impl Into<NodeKind>) -> Node {
        Node {
            kind: kind.into(),
            span,
            type_,
        }
    }

    fn lower(&mut self, node: &syntax::Node) -> Result<Node> {
        match &node.kind {
            syntax::NodeKind::Define(define) => self.define(node, define),
            syntax::NodeKind::Unary(unary) => self.unary(node, unary),
            syntax::NodeKind::Binary(binary) => self.binary(node, binary),
            syntax::NodeKind::Name(name) => self.name(node, name),
            syntax::NodeKind::Number(literal) => self.number(node, literal),
            syntax::NodeKind::Integer(literal) => self.integer(node, literal),
        }
    }

    fn define(&mut self, node: &syntax::Node, definition: &syntax::Define) -> Result<Node> {
        let value = self.lower(&definition.value)?;
        let type_ = value.type_;
        let symbol = self.state.define(definition.name.value.clone(), type_);

        Ok(self.node(node.span, type_, Define { symbol, value }))
    }

    fn name(&mut self, node: &syntax::Node, name: &syntax::Name) -> Result<Node> {
        let Some(symbol) = self.state.symbol(&name.value) else {
            return self.fail(node.span, format!("`{}` not defined", name.value));
        };

        Ok(match symbol.symbol_id {
            state::TRUE => self.node(node.span, symbol.type_, Boolean { value: true }),
            state::FALSE => self.node(node.span, symbol.type_, Boolean { value: false }),
            _ => {
                self.node(
                    node.span,
                    symbol.type_,
                    Local {
                        symbol_id: symbol.symbol_id,
                    },
                )
            }
        })
    }

    fn binary(&mut self, node: &syntax::Node, binary: &syntax::Binary) -> Result<Node> {
        let loperand = self.lower(&binary.lexpr)?;
        let roperand = self.lower(&binary.rexpr)?;

        let operator = match binary.operator.kind {
            syntax::OperatorKind::Add => Operator::Add,
            syntax::OperatorKind::Sub => Operator::Sub,
            syntax::OperatorKind::Mul => Operator::Mul,
            syntax::OperatorKind::Div => Operator::Div,
            syntax::OperatorKind::Mod => Operator::Mod,
            syntax::OperatorKind::Pow => Operator::Pow,
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

        let type_ = match operator {
            Operator::And | Operator::Or => {
                self.expect_type(&loperand, types::BOOLEAN)?;
                self.expect_type(&roperand, types::BOOLEAN)?;
                types::BOOLEAN
            }
            Operator::Eq | Operator::Ne => {
                match loperand.type_ {
                    types::BOOLEAN => self.expect_type(&roperand, types::BOOLEAN)?,
                    types::INTEGER => self.expect_type(&roperand, types::NUMBER)?,
                    types::NUMBER => self.expect_type(&roperand, types::NUMBER)?,
                    _ => {
                        return self.fail(node.span, "invalid type for equality comparison");
                    }
                }
                types::BOOLEAN
            }
            Operator::Lt | Operator::Le | Operator::Gt | Operator::Ge => {
                self.expect_type(&loperand, types::NUMBER)?;
                self.expect_type(&roperand, types::NUMBER)?;
                types::BOOLEAN
            }
            Operator::Add | Operator::Sub | Operator::Mul | Operator::Pow => {
                self.expect_type(&loperand, types::NUMBER)?;
                self.expect_type(&roperand, types::NUMBER)?;

                if loperand.type_ == types::INTEGER && roperand.type_ == types::INTEGER {
                    types::INTEGER
                } else {
                    types::NUMBER
                }
            }
            Operator::Div => {
                self.expect_type(&loperand, types::NUMBER)?;
                self.expect_type(&roperand, types::NUMBER)?;
                types::NUMBER
            }
            Operator::Mod => {
                self.expect_type(&loperand, types::INTEGER)?;
                self.expect_type(&roperand, types::INTEGER)?;
                types::INTEGER
            }
            _ => {
                return self.fail(node.span, "invalid binary operator");
            }
        };

        Ok(self.node(
            node.span,
            type_,
            Binary {
                operator,
                loperand,
                roperand,
            },
        ))
    }

    fn unary(&mut self, node: &syntax::Node, unary: &syntax::Unary) -> Result<Node> {
        let operand = self.lower(&unary.expr)?;

        let operator = match unary.operator.kind {
            syntax::OperatorKind::Pos => Operator::Pos,
            syntax::OperatorKind::Neg => Operator::Neg,
            syntax::OperatorKind::Not => Operator::Not,
            _ => {
                return self.fail(unary.operator.span, "unsupported unary operator");
            }
        };

        let type_ = match operator {
            Operator::Not => {
                self.expect_type(&operand, types::BOOLEAN)?;
                types::BOOLEAN
            }
            Operator::Pos | Operator::Neg => {
                self.expect_type(&operand, types::NUMBER)?;
                operand.type_
            }
            _ => {
                return self.fail(node.span, "invalid unary operator");
            }
        };

        Ok(self.node(
            node.span,
            type_,
            Unary {
                operator,
                operand,
            },
        ))
    }

    fn number(&mut self, node: &syntax::Node, number: &syntax::Literal) -> Result<Node> {
        let Ok(value) = str::parse(&number.value) else {
            return self.fail(node.span, "value is not supported as an number");
        };

        Ok(self.node(node.span, types::NUMBER, Number { value }))
    }

    fn integer(&mut self, node: &syntax::Node, integer: &syntax::Literal) -> Result<Node> {
        let value = match &integer.value.get(0..2) {
            Some("0b") => i64::from_str_radix(&integer.value[2..], 2),
            Some("0x") => i64::from_str_radix(&integer.value[2..], 16),
            _ => str::parse(&integer.value),
        };

        let Ok(value) = value else {
            return self.fail(node.span, "value is not supported as an integer");
        };

        Ok(self.node(node.span, types::INTEGER, Integer { value }))
    }

    fn expect_type(&self, node: &Node, expected_type: Type) -> Result<()> {
        if node.type_.is(expected_type) {
            Ok(())
        } else {
            self.fail(node.span, format!("expected `{}`, found `{}`", expected_type, node.type_))
        }
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }
}

pub fn from_syntax(module: &syntax::Module, state: &mut State) -> Result<Module> {
    let mut lower = LowerSyntax::new(state);
    let mut nodes = vec![];

    for node in &module.nodes {
        nodes.push(lower.lower(node)?);
    }

    Ok(Module { nodes })
}

#[cfg(test)]
mod tests {}

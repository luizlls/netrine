use std::fmt::{self, Display};

use crate::error::{Error, Result};
use crate::pprint::{PrettyPrint, PrettyPrintNode, PrettyPrinter};
use crate::source::{Span, ToSpan};
use crate::syntax;
use crate::types::{self, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub(crate) nodes: Vec<Node>,
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
    Number(Number),
    Integer(Integer),
}

impl ToSpan for Node {
    fn span(&self) -> Span {
        match self {
            Node::Unary(unary) => unary.span,
            Node::Binary(binary) => binary.span,
            Node::Number(number) => number.span,
            Node::Integer(integer) => integer.span,
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
            Node::Number(number) => number.print(),
            Node::Integer(integer) => integer.print(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub(crate) operator: Operator,
    pub(crate) operand: Node,
    pub(crate) span: Span,
    pub(crate) type_: Type,
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pprint(f)
    }
}

impl PrettyPrint for Unary {
    fn print(&self) -> PrettyPrintNode<'_> {
        PrettyPrintNode::printer()
            .label(format!("UNARY: {}", self.type_))
            .child(&self.operator)
            .child(&self.operand)
            .print()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub(crate) operator: Operator,
    pub(crate) loperand: Node,
    pub(crate) roperand: Node,
    pub(crate) span: Span,
    pub(crate) type_: Type,
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pprint(f)
    }
}

impl PrettyPrint for Binary {
    fn print(&self) -> PrettyPrintNode<'_> {
        PrettyPrintNode::printer()
            .label(format!("BINARY: {}", self.type_))
            .child(&self.loperand)
            .child(&self.operator)
            .child(&self.roperand)
            .print()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Integer {
    pub(crate) value: i64,
    pub(crate) span: Span,
}

impl Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pprint(f)
    }
}

impl PrettyPrint for Integer {
    fn print(&self) -> PrettyPrintNode<'_> {
        PrettyPrintNode::printer()
            .label(format!("INTEGER({}): {}", self.value, types::INTEGER))
            .print()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub(crate) value: f64,
    pub(crate) span: Span,
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pprint(f)
    }
}

impl PrettyPrint for Number {
    fn print(&self) -> PrettyPrintNode<'_> {
        PrettyPrintNode::printer()
            .label(format!("NUMBER({}): {}", self.value, types::NUMBER))
            .print()
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

impl PrettyPrint for Operator {
    fn print(&self) -> PrettyPrintNode<'_> {
        PrettyPrintNode::printer()
            .label(format!("OPERATOR({})", self))
            .print()
    }
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

struct LowerSyntax {}

impl LowerSyntax {
    fn new() -> LowerSyntax {
        LowerSyntax {}
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }

    fn node(&mut self, node: &syntax::Node) -> Result<Node> {
        match node {
            syntax::Node::Def(def) => todo!(),
            syntax::Node::Unary(unary) => self.unary(unary),
            syntax::Node::Binary(binary) => self.binary(binary),
            syntax::Node::Name(literal) => todo!(),
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

        Ok(Node::Binary(
            Binary {
                operator,
                loperand,
                roperand,
                type_: Type::Unknown,
                span: binary.span,
            }
            .into(),
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
            }
            .into(),
        ))
    }

    fn number(&mut self, number: &syntax::Literal) -> Result<Node> {
        let Ok(value) = str::parse(&number.value) else {
            return self.fail(number.span, "value is not supported as an number");
        };

        Ok(Node::Number(Number {
            value,
            span: number.span,
        }))
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

        Ok(Node::Integer(Integer {
            value,
            span: integer.span,
        }))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::*;
    use crate::source::*;
    use crate::syntax;

    #[test]
    fn invalid_number() {
        let module = syntax::Module {
            nodes: vec![syntax::Node::Number(syntax::Literal {
                value: "12f".into(),
                span: Span::new(0, 3),
            })],
        };

        let error = from_syntax(&module).unwrap_err();

        assert_eq!(
            error,
            Error::error(Span::new(0, 3), "value is not supported as an number".into())
        );
    }

    #[test]
    fn invalid_binary_integer() {
        let module = syntax::Module {
            nodes: vec![syntax::Node::Integer(syntax::Literal {
                value: "0b012".into(),
                span: Span::new(0, 5),
            })],
        };

        let error = from_syntax(&module).unwrap_err();

        assert_eq!(
            error,
            Error::error(Span::new(0, 5), "value is not supported as an integer".into())
        );
    }

    #[test]
    fn invalid_hex_integer() {
        let module = syntax::Module {
            nodes: vec![syntax::Node::Integer(syntax::Literal {
                value: "0xfgh".into(),
                span: Span::new(0, 5),
            })],
        };

        let error = from_syntax(&module).unwrap_err();

        assert_eq!(
            error,
            Error::error(Span::new(0, 5), "value is not supported as an integer".into())
        );
    }

    #[test]
    fn invalid_binary_operator() {
        let module = syntax::Module {
            nodes: vec![syntax::Node::Binary(
                syntax::Binary {
                    operator: syntax::Operator {
                        kind: syntax::OperatorKind::Not,
                        span: Span::new(1, 2),
                    },
                    lexpr: syntax::Node::Integer(syntax::Literal {
                        value: "1".into(),
                        span: Span::new(0, 1),
                    }),
                    rexpr: syntax::Node::Integer(syntax::Literal {
                        value: "2".into(),
                        span: Span::new(2, 3),
                    }),
                    span: Span::new(0, 3),
                }
                .into(),
            )],
        };

        let error = from_syntax(&module).unwrap_err();

        assert_eq!(error, Error::error(Span::new(1, 2), "unsupported binary operator".into()));
    }

    #[test]
    fn invalid_unary_operator() {
        let module = syntax::Module {
            nodes: vec![syntax::Node::Unary(
                syntax::Unary {
                    operator: syntax::Operator {
                        kind: syntax::OperatorKind::Add,
                        span: Span::new(0, 1),
                    },
                    expr: syntax::Node::Integer(syntax::Literal {
                        value: "1".into(),
                        span: Span::new(1, 2),
                    }),
                    span: Span::new(0, 2),
                }
                .into(),
            )],
        };

        let error = from_syntax(&module).unwrap_err();

        assert_eq!(error, Error::error(Span::new(0, 1), "unsupported unary operator".into()));
    }
}

use std::fmt::{self, Display};

use crate::collections::IndexMap;
use crate::error::{Error, Result};
use crate::interner::{Interner, Name};
use crate::macros::entity_id;
use crate::source::{Source, Span};
use crate::syntax;
use crate::types::{self, TypeId};

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub type_id: TypeId,
}

impl Node {
    pub fn new(kind: NodeKind, span: Span, type_id: TypeId) -> Node {
        Node {
            kind,
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Global(Box<Global>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    LocalRef(LocalRef),
    GlobalRef(GlobalRef),
    Integer(Integer),
    Number(Number),
    True,
    False,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
    pub name: Name,
    pub value: Node,
}

impl Node {
    pub fn global(name: Name, value: Node, span: Span, type_id: TypeId) -> Node {
        Node {
            kind: NodeKind::Global(Global { name, value }.into()),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalRef {
    pub name: Name,
}

impl Node {
    pub fn local_ref(name: Name, span: Span, type_id: TypeId) -> Node {
        Node {
            kind: NodeKind::LocalRef(LocalRef { name }),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalRef {
    pub name: Name,
}

impl Node {
    pub fn global_ref(name: Name, span: Span, type_id: TypeId) -> Node {
        Node {
            kind: NodeKind::GlobalRef(GlobalRef { name }),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: Operator,
    pub operand: Node,
}

impl Node {
    pub fn unary(operator: Operator, operand: Node, span: Span, type_id: TypeId) -> Node {
        Node {
            kind: NodeKind::Unary(
                Unary {
                    operator,
                    operand,
                }
                .into(),
            ),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub operator: Operator,
    pub loperand: Node,
    pub roperand: Node,
}

impl Node {
    pub fn binary(
        operator: Operator,
        loperand: Node,
        roperand: Node,
        span: Span,
        type_id: TypeId,
    ) -> Node {
        Node {
            kind: NodeKind::Binary(
                Binary {
                    operator,
                    loperand,
                    roperand,
                }
                .into(),
            ),
            span,
            type_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Integer {
    pub value: i64,
}

impl Node {
    pub fn integer(value: i64, span: Span) -> Node {
        Node {
            kind: NodeKind::Integer(Integer { value }),
            span,
            type_id: types::INTEGER,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub value: f64,
}

impl Node {
    pub fn number(value: f64, span: Span) -> Node {
        Node {
            kind: NodeKind::Number(Number { value }),
            span,
            type_id: types::NUMBER,
        }
    }
}

impl Node {
    pub fn bool(truthy: bool, span: Span) -> Node {
        Node {
            kind: if truthy {
                NodeKind::True
            } else {
                NodeKind::False
            },
            span,
            type_id: types::BOOLEAN,
        }
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

entity_id!(SymbolId, u32);

#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub kind: SymbolKind,
    pub name: Name,
    pub span: Span,
    pub type_id: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Local,
    Global,
}

#[derive(Debug)]
pub struct Symbols {
    pub symbols: IndexMap<Name, SymbolId, Symbol>,
}

impl Symbols {
    pub fn new() -> Symbols {
        Symbols {
            symbols: IndexMap::new(),
        }
    }

    pub fn define(
        &mut self,
        name: Name,
        kind: SymbolKind,
        span: Span,
        type_id: TypeId,
    ) -> SymbolId {
        self.symbols.insert_with(name, |id| {
            Symbol {
                id,
                name,
                kind,
                span,
                type_id,
            }
        })
    }

    pub fn symbol(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get_by_id(id)
    }

    pub fn lookup(&self, name: Name) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

#[derive(Debug)]
struct LowerSyntax<'hir> {
    source: &'hir Source,
    interner: &'hir mut Interner,
    symbols: Symbols,
}

impl<'hir> LowerSyntax<'hir> {
    fn new(source: &'hir Source, interner: &'hir mut Interner) -> LowerSyntax<'hir> {
        LowerSyntax {
            source,
            interner,
            symbols: Symbols::new(),
        }
    }

    fn intern(&mut self, name: &syntax::Name) -> Name {
        self.interner.intern(self.source.slice(name.span))
    }

    fn lower(&mut self, node: &syntax::Node) -> Result<Node> {
        match &node.kind {
            syntax::NodeKind::Define(define) => self.define(node, define),
            syntax::NodeKind::Unary(unary) => self.unary(node, unary),
            syntax::NodeKind::Binary(binary) => self.binary(node, binary),
            syntax::NodeKind::Name(name) => self.name(node, name),
            syntax::NodeKind::Number => self.number(node),
            syntax::NodeKind::Integer => self.integer(node),
            syntax::NodeKind::True => self.bool(node, true),
            syntax::NodeKind::False => self.bool(node, false),
        }
    }

    fn define(&mut self, node: &syntax::Node, def: &syntax::Define) -> Result<Node> {
        let name = self.intern(&def.name);

        if self.symbols.lookup(name).is_some() {
            return self.fail(node.span, format!("`{}` is already defined", &self.interner[name]));
        };

        let value = self.lower(&def.value)?;
        let type_id = value.type_id;

        self.symbols.define(name, SymbolKind::Global, node.span, type_id);

        Ok(Node::global(name, value, node.span, type_id))
    }

    fn name(&mut self, node: &syntax::Node, name: &syntax::Name) -> Result<Node> {
        let name = self.intern(name);

        let Some(symbol) = self.symbols.lookup(name) else {
            return self.fail(node.span, format!("`{}` is not defined", &self.interner[name]));
        };

        Ok(Node::global_ref(name, node.span, symbol.type_id))
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
                return self.fail(node.span, "unsupported binary operator");
            }
        };

        let type_id = match operator {
            Operator::And | Operator::Or => {
                self.expect_type(&loperand, types::BOOLEAN)?;
                self.expect_type(&roperand, types::BOOLEAN)?;
                types::BOOLEAN
            }
            Operator::Eq | Operator::Ne => {
                match loperand.type_id {
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

                if loperand.type_id == types::INTEGER && roperand.type_id == types::INTEGER {
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

        Ok(Node::binary(operator, loperand, roperand, node.span, type_id))
    }

    fn unary(&mut self, node: &syntax::Node, unary: &syntax::Unary) -> Result<Node> {
        let operand = self.lower(&unary.expr)?;

        let operator = match unary.operator.kind {
            syntax::OperatorKind::Pos => Operator::Pos,
            syntax::OperatorKind::Neg => Operator::Neg,
            syntax::OperatorKind::Not => Operator::Not,
            _ => {
                return self.fail(node.span, "unsupported unary operator");
            }
        };

        let type_id = match operator {
            Operator::Not => {
                self.expect_type(&operand, types::BOOLEAN)?;
                types::BOOLEAN
            }
            Operator::Pos | Operator::Neg => {
                self.expect_type(&operand, types::NUMBER)?;
                operand.type_id
            }
            _ => {
                return self.fail(node.span, "invalid unary operator");
            }
        };

        Ok(Node::unary(operator, operand, node.span, type_id))
    }

    fn number(&mut self, node: &syntax::Node) -> Result<Node> {
        let value = self.source.slice(node.span);

        let Ok(value) = str::parse(&value) else {
            return self.fail(node.span, "value is not supported as an number");
        };

        Ok(Node::number(value, node.span))
    }

    fn integer(&mut self, node: &syntax::Node) -> Result<Node> {
        let value = self.source.slice(node.span);

        let value = match &value.get(0..2) {
            Some("0b") => i64::from_str_radix(&value[2..], 2),
            Some("0x") => i64::from_str_radix(&value[2..], 16),
            _ => str::parse(&value),
        };

        let Ok(value) = value else {
            return self.fail(node.span, "value is not supported as an integer");
        };

        Ok(Node::integer(value, node.span))
    }

    fn bool(&mut self, node: &syntax::Node, truthy: bool) -> Result<Node> {
        Ok(Node::bool(truthy, node.span))
    }

    fn expect_type(&self, node: &Node, expected_type: TypeId) -> Result<()> {
        if node.type_id.is(expected_type) {
            Ok(())
        } else {
            self.fail(node.span, format!("expected `{}`, found `{}`", expected_type, node.type_id))
        }
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }
}

pub fn from_syntax(
    module: &syntax::Module,
    source: &Source,
    interner: &mut Interner,
) -> Result<Module> {
    let mut lower = LowerSyntax::new(source, interner);
    let mut nodes = vec![];

    for node in &module.nodes {
        nodes.push(lower.lower(node)?);
    }

    Ok(Module { nodes })
}

use std::fmt::{self, Display};

use crate::error::{Error, Result};
use crate::pprint::{PrettyPrint, PrettyPrintNode};
use crate::source::Span;
use crate::state::State;
use crate::syntax::{self, NodeKind};
use crate::type_check::Types;
use crate::types::{self, Type};

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) instructions: Vec<Instruction>,
}

impl Module {
    pub fn get_type(&self, instruction_id: &InstructionId) -> Type {
        self.instructions[instruction_id.id() as usize].type_
    }
}

impl PrettyPrint for Module {
    fn print(&self, _state: &State) -> PrettyPrintNode<'_> {
        let mut printer = PrettyPrintNode::printer();

        for (idx, instruction) in self.instructions.iter().enumerate() {
            printer.add_label(format!("v{}: {} = {}", idx, instruction.type_, instruction.kind));
        }

        printer.print()
    }
}

#[derive(Debug, Clone, Default)]
pub struct Block {
    pub(crate) instructions: Vec<InstructionId>,
}

impl Block {
    pub fn new() -> Block {
        Block {
            instructions: vec![],
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub(crate) u32);

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub(crate) kind: InstructionKind,
    pub(crate) type_: Type,
    pub(crate) block: BlockId,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct InstructionId(pub(crate) u32);

impl InstructionId {
    #[inline(always)]
    pub fn id(self) -> u32 {
        self.0
    }
}

impl Display for InstructionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    Unary(Unary),
    Binary(Binary),
    Number(Number),
    Integer(Integer),
}

impl Display for InstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            InstructionKind::Unary(unary) => write!(f, "{unary}"),
            InstructionKind::Binary(binary) => write!(f, "{binary}"),
            InstructionKind::Number(number) => write!(f, "{number}"),
            InstructionKind::Integer(integer) => write!(f, "{integer}"),
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
        let s = match self {
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
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub(crate) operator: Operator,
    pub(crate) operand: InstructionId,
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.operator, self.operand)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub(crate) operator: Operator,
    pub(crate) loperand: InstructionId,
    pub(crate) roperand: InstructionId,
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.operator, self.loperand, self.roperand)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Integer {
    pub(crate) value: i64,
}

impl Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub(crate) value: f64,
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

struct LowerSyntax<'a> {
    instructions: Vec<Instruction>,
    block: BlockId,
    types: &'a Types,
}

impl<'a> LowerSyntax<'a> {
    fn new(types: &'a Types) -> LowerSyntax<'a> {
        LowerSyntax {
            instructions: vec![],
            types,
            block: BlockId(0),
        }
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }

    fn emit(&mut self, kind: InstructionKind, type_: Type) -> InstructionId {
        let instruction_id = InstructionId(self.instructions.len() as u32);

        self.instructions.push(Instruction {
            kind,
            type_,
            block: self.block,
        });

        instruction_id
    }

    fn node(&mut self, node: &syntax::Node) -> Result<InstructionId> {
        match &node.kind {
            NodeKind::Define(_) => todo!(),
            NodeKind::Binary(binary) => self.binary(node, binary),
            NodeKind::Unary(unary) => self.unary(node, unary),
            NodeKind::Name(_) => todo!(),
            NodeKind::Number(literal) => self.number(node, literal),
            NodeKind::Integer(literal) => self.integer(node, literal),
        }
    }

    fn binary(&mut self, node: &syntax::Node, binary: &syntax::Binary) -> Result<InstructionId> {
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

        Ok(self.emit(
            InstructionKind::Binary(Binary {
                operator,
                loperand,
                roperand,
            }),
            self.types.get(node.id),
        ))
    }

    fn unary(&mut self, node: &syntax::Node, unary: &syntax::Unary) -> Result<InstructionId> {
        let operand = self.node(&unary.expr)?;

        let operator = match unary.operator.kind {
            syntax::OperatorKind::Pos => Operator::Pos,
            syntax::OperatorKind::Neg => Operator::Neg,
            syntax::OperatorKind::Not => Operator::Not,
            _ => {
                return self.fail(unary.operator.span, "unsupported unary operator");
            }
        };

        Ok(self.emit(
            InstructionKind::Unary(Unary {
                operator,
                operand,
            }),
            self.types.get(node.id),
        ))
    }

    fn number(&mut self, node: &syntax::Node, literal: &syntax::Literal) -> Result<InstructionId> {
        let Ok(value) = str::parse(&literal.value) else {
            return self.fail(node.span, "value is not supported as a number");
        };

        Ok(self.emit(InstructionKind::Number(Number { value }), types::NUMBER))
    }

    fn integer(&mut self, node: &syntax::Node, literal: &syntax::Literal) -> Result<InstructionId> {
        let value = match literal.value.get(0..2) {
            Some("0b") => i64::from_str_radix(&literal.value[2..], 2),
            Some("0x") => i64::from_str_radix(&literal.value[2..], 16),
            _ => str::parse(&literal.value),
        };

        let Ok(value) = value else {
            return self.fail(node.span, "value is not supported as an integer");
        };

        Ok(self.emit(InstructionKind::Integer(Integer { value }), types::INTEGER))
    }
}

pub fn from_syntax(module: &syntax::Module, types: &Types) -> Result<Module> {
    let mut lower = LowerSyntax::new(types);

    for node in &module.nodes {
        lower.node(node)?;
    }

    Ok(Module {
        instructions: lower.instructions,
    })
}

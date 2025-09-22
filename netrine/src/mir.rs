use std::fmt::{self, Display};

use crate::error::Result;
use crate::types::Type;
use crate::{hir, types};

#[derive(Debug, Clone)]
pub struct Module {
    pub instructions: Vec<Instruction>,
}

impl Module {
    pub fn get_type(&self, instruction_id: &InstructionId) -> Type {
        self.instructions[instruction_id.id() as usize].type_
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, instruction) in self.instructions.iter().enumerate() {
            writeln!(f, "v{idx} := {instruction}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Block {
    pub instructions: Vec<InstructionId>,
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

impl BlockId {
    #[inline(always)]
    pub fn id(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub type_: Type,
    pub block: BlockId,
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

#[derive(Debug, Clone)]
pub enum InstructionKind {
    Unary(Unary),
    Binary(Binary),
    Number(Number),
    Integer(Integer),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            InstructionKind::Unary(unary) => write!(f, "{unary}"),
            InstructionKind::Binary(binary) => write!(f, "{binary}"),
            InstructionKind::Number(number) => write!(f, "{number}"),
            InstructionKind::Integer(integer) => write!(f, "{integer}"),
        }
    }
}

pub type Operator = crate::hir::Operator;

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub operand: InstructionId,
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.operator, self.operand)
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub loperand: InstructionId,
    pub roperand: InstructionId,
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.operator, self.loperand, self.roperand)
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct Number {
    pub value: f64,
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

struct LowerHir {
    instructions: Vec<Instruction>,
    block: BlockId,
}

impl LowerHir {
    fn new() -> LowerHir {
        LowerHir {
            instructions: vec![],
            block: BlockId(0),
        }
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

    fn node(&mut self, node: &hir::Node) -> Result<InstructionId> {
        match node {
            hir::Node::Binary(node) => self.binary(node),
            hir::Node::Unary(node) => self.unary(node),
            hir::Node::Number(literal) => self.number(literal),
            hir::Node::Integer(literal) => self.integer(literal),
        }
    }

    fn binary(&mut self, binary: &hir::Binary) -> Result<InstructionId> {
        let loperand = self.node(&binary.loperand)?;
        let roperand = self.node(&binary.roperand)?;

        Ok(self.emit(
            InstructionKind::Binary(Binary {
                operator: binary.operator,
                loperand,
                roperand,
            }),
            binary.type_,
        ))
    }

    fn unary(&mut self, unary: &hir::Unary) -> Result<InstructionId> {
        let operand = self.node(&unary.operand)?;

        Ok(self.emit(
            InstructionKind::Unary(Unary {
                operator: unary.operator,
                operand,
            }),
            unary.type_,
        ))
    }

    fn number(&mut self, number: &hir::Number) -> Result<InstructionId> {
        Ok(self.emit(
            InstructionKind::Number(Number {
                value: number.value,
            }),
            types::NUMBER,
        ))
    }

    fn integer(&mut self, integer: &hir::Integer) -> Result<InstructionId> {
        Ok(self.emit(
            InstructionKind::Integer(Integer {
                value: integer.value,
            }),
            types::INTEGER,
        ))
    }
}

pub fn from_hir(module: &hir::Module) -> Result<Module> {
    let mut lower = LowerHir::new();

    for node in &module.nodes {
        lower.node(node)?;
    }

    Ok(Module {
        instructions: lower.instructions,
    })
}

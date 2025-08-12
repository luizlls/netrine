use std::fmt::{self, Display};

use crate::semantics::{Type, Operator};

#[derive(Debug, Clone)]
pub struct Module {
    pub instructions: Vec<Instruction>,
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

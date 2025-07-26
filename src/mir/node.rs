use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct TypeId(pub u32);

impl TypeId {
    pub fn id(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for TypeId {
    fn from(i: usize) -> Self {
        TypeId(i as u32)
    }
}

impl From<TypeId> for usize {
    fn from(value: TypeId) -> Self {
        value.id()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Number,
    Integer,
    Boolean,
}

pub const TYPE_UNKNOWN: TypeId = TypeId(0);
pub const TYPE_NUMBER: TypeId = TypeId(1);
pub const TYPE_INTEGER: TypeId = TypeId(2);
pub const TYPE_BOOL: TypeId = TypeId(3);

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for function in &self.functions {
            writeln!(f, "{function}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub instructions: Vec<Instruction>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    pub fn id(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub type_id: TypeId,
    pub block_id: BlockId,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct InstructionId(pub(crate) u32);

impl InstructionId {
    #[inline(always)]
    pub fn id(self) -> usize {
        self.0 as usize
    }
}

impl Display for InstructionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.operator, self.loperand, self.roperand)
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct Number {
    pub value: f64,
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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

use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct TypeId(pub u32);

impl TypeId {
    #[inline(always)]
    pub fn id(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for TypeId {
    fn from(i: usize) -> Self {
        TypeId(i as u32)
    }
}

impl Into<usize> for TypeId {
    fn into(self) -> usize {
        self.id()
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct VariableId(pub u32);

impl VariableId {
    #[inline(always)]
    pub fn id(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for VariableId {
    fn from(i: usize) -> Self {
        VariableId(i as u32)
    }
}

impl Into<usize> for VariableId {
    fn into(self) -> usize {
        self.id()
    }
}

impl Display for VariableId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.id())
    }
}

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
    pub block: Block,
}

impl Function {
    pub fn new() -> Function {
        Function {
            block: Block::new(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.block)
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub instructions: Vec<Instruction>,
}

impl Block {
    pub fn new() -> Block {
        Block { instructions: Vec::new() }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for inst in &self.instructions {
            writeln!(f, "{inst}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Unary(Unary),
    Binary(Binary),
    Number(Number),
    Integer(Integer),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Unary(unary) => write!(f, "{unary}"),
            Instruction::Binary(binary) => write!(f, "{binary}"),
            Instruction::Number(number) => write!(f, "{number}"),
            Instruction::Integer(integer) => write!(f, "{integer}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub target: VariableId,
    pub operator: Operator,
    pub operand: VariableId,
}

impl Display for Unary {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} := {} {}", self.target, self.operator, self.operand)
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub target: VariableId,
    pub operator: Operator,
    pub loperand: VariableId,
    pub roperand: VariableId,
}

impl Display for Binary {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} := {} {} {}", self.target, self.operator, self.loperand, self.roperand)
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub target: VariableId,
    pub value: i64,
}

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} := {}", self.target, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct Number {
    pub target: VariableId,
    pub value: f64,
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} := {}", self.target, self.value)
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

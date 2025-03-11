use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct VariableId(u32);

impl VariableId {
    pub fn new(index: u32) -> VariableId {
        VariableId(index)
    }
}

impl Display for VariableId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub blocks: Vec<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub instructions: Vec<Instruction>,
}

impl Block {
    pub fn new() -> Block {
        Block {
            instructions: Vec::new(),
        }
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct VariableId(u32);

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct BlockId(u32);

#[derive(Debug, Clone)]
pub struct Module {
    pub blocks: Vec<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    var: VariableId,
    kind: InstructionKind,
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
    Unary(Unary),
    Binary(Binary),
    Number(Literal),
    Integer(Literal),
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Operand,
    pub rexpr: Operand,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Operand,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: Operand,
}

#[derive(Debug, Clone)]
pub enum Operator {
    Is,
    In,
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
    Range,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Number(f64),
    Integer(i64),
    Boolean(bool),
    Variable(VariableId),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct VariableId(pub u32);

impl VariableId {
    pub fn next(self) -> VariableId {
        VariableId(self.0 + 1)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub u32);

impl BlockId {
    pub fn next(self) -> BlockId {
        BlockId(self.0 + 1)
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub blocks: Vec<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
}

impl Block {
    pub fn new(id: BlockId) -> Block {
        Block {
            id,
            instructions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    var: VariableId,
    kind: InstructionKind,
}

impl Instruction {
    pub fn new(var: VariableId, kind: InstructionKind) -> Self {
        Self { var, kind }
    }
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

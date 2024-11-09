#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct ConstantId(u32);

impl From<usize> for ConstantId {
    fn from(value: usize) -> ConstantId {
        ConstantId(value as u32)
    }
}

impl From<ConstantId> for usize {
    fn from(value: ConstantId) -> usize {
        value.0 as usize
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct FunctionId(u32);

impl From<usize> for FunctionId {
    fn from(value: usize) -> FunctionId {
        FunctionId(value as u32)
    }
}

impl From<FunctionId> for usize {
    fn from(value: FunctionId) -> usize {
        value.0 as usize
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct VariableId(u32);

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct BlockId(u32);

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
    pub constants: Vec<Constant>,
}

#[derive(Debug, Clone)]
pub enum Constant {
    String(String),
    Number(f64),
    Integer(i64),
    Boolean(bool),
    Block(Block),
    Reference(Reference),
}

#[derive(Debug, Clone)]
pub enum Reference {
    Constant(ConstantId),
    Function(FunctionId),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub parameters: Vec<VariableId>,
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
    Member(Member),
    Index(Index),
    Apply(Apply),
    Unary(Unary),
    Binary(Binary),
    Array(Array),
    Tuple(Tuple),
    String(Literal),
    Number(Literal),
    Integer(Literal),
    Return(Return),
}

#[derive(Debug, Clone)]
pub struct Member {
    pub source: VariableId,
    pub field: Operand,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub source: VariableId,
    pub index: u32,
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub callee: Operand,
    pub arguments: Vec<Operand>,
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
pub struct Array {
    pub items: Vec<Operand>,
}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub items: Vec<Operand>,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: Operand,
}

#[derive(Debug, Clone)]
pub struct Return {
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
    Reference(Reference),
}

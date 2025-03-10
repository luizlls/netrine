#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct VariableId(u32);

impl VariableId {
    pub fn new(index: u32) -> VariableId {
        VariableId(index)
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

    pub fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Unary(Unary),
    Binary(Binary),
    Number(Number),
    Integer(Integer),
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub dest: VariableId,
    pub operator: Operator,
    pub operand: Operand,
}

impl Instruction {
    pub fn unary(dest: VariableId, operator: Operator, operand: Operand) -> Instruction {
        Instruction::Unary(
            Unary {
                dest,
                operator,
                operand,
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub dest: VariableId,
    pub operator: Operator,
    pub loperand: Operand,
    pub roperand: Operand,
}

impl Instruction {
    pub fn binary(dest: VariableId, operator: Operator, loperand: Operand, roperand: Operand) -> Instruction {
        Instruction::Binary(
            Binary {
                dest,
                operator,
                loperand,
                roperand,
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub dest: VariableId,
    pub value: i64,
}

impl Instruction {
    pub fn integer(dest: VariableId, value: i64) -> Instruction {
        Instruction::Integer(
            Integer {
                dest,
                value,
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Number {
    pub dest: VariableId,
    pub value: f64,
}

impl Instruction {
    pub fn number(dest: VariableId, value: f64) -> Instruction {
        Instruction::Number(
            Number {
                dest,
                value,
            }
        )
    }
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

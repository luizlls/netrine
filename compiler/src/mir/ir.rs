use crate::collections::{IndexMap, IndexVec};
use crate::interner::Name;
use crate::macros::entity_id;
use crate::source::Span;
use crate::types::TypeId;

#[derive(Debug)]
pub struct Module {
    pub definitions: IndexMap<Name, DefinitionId, Definition>,
    pub functions: IndexMap<Name, FunctionId, Function>,
    pub entrypoint: Function,
}

entity_id!(InstructionId, u32);

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Parameter(Parameter),
    Global(GlobalRef),
    Binary(Binary),
    Unary(Unary),
    Integer(Integer),
    Number(Number),
    True,
    False,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    And, // and
    Or,  // or
    Not, // not
    Pos, // +
    Neg, // -
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Pow, // ^
    Eq,  // ==
    Ne,  // !=
    Lt,  // <
    Le,  // <=
    Gt,  // >
    Ge,  // >=
}

#[derive(Debug, Clone, Copy)]
pub struct Parameter {
    pub position: u8,
}

impl Instruction {
    pub const fn parameter(position: u8) -> Instruction {
        Instruction::Parameter(Parameter { position })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalRef {
    pub definition_id: DefinitionId,
}

impl Instruction {
    pub const fn global(definition_id: DefinitionId) -> Instruction {
        Instruction::Global(GlobalRef { definition_id })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Unary {
    pub operator: Operator,
    pub operand: InstructionId,
}

impl Instruction {
    pub const fn unary(operator: Operator, operand: InstructionId) -> Instruction {
        Instruction::Unary(Unary {
            operator,
            operand,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Binary {
    pub operator: Operator,
    pub loperand: InstructionId,
    pub roperand: InstructionId,
}

impl Instruction {
    pub const fn binary(operator: Operator, loperand: InstructionId, roperand: InstructionId) -> Instruction {
        Instruction::Binary(Binary {
            operator,
            loperand,
            roperand,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Integer {
    pub value: i64,
}

impl Instruction {
    pub const fn integer(value: i64) -> Instruction {
        Instruction::Integer(Integer { value })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Number {
    pub value: f64,
}

impl Instruction {
    pub const fn number(value: f64) -> Instruction {
        Instruction::Number(Number { value })
    }
}

entity_id!(DefinitionId, u32);

#[derive(Debug)]
pub struct Definition {
    pub instructions: IndexVec<InstructionId, Instruction>,
    pub types: IndexVec<InstructionId, TypeId>,
    pub spans: IndexVec<InstructionId, Span>,
}

entity_id!(FunctionId, u32);

#[derive(Debug)]
pub struct Function {
    pub instructions: IndexVec<InstructionId, Instruction>,
    pub types: IndexVec<InstructionId, TypeId>,
    pub spans: IndexVec<InstructionId, Span>,
}

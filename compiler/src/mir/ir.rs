use crate::collections::{IndexMap, IndexVec};
use crate::interner::Name;
use crate::macros::entity_id;
use crate::source::Span;
use crate::types::TypeId;

#[derive(Debug)]
pub struct Module {
    pub instructions: IndexVec<InstructionId, Instruction>,
    pub types: IndexVec<InstructionId, TypeId>,
    pub spans: IndexVec<InstructionId, Span>,
    pub instruction_lists: IndexVec<InstructionListId, InstructionList>,
    pub blocks: IndexVec<BlockId, Block>,
    pub definitions: IndexMap<Name, DefinitionId, Definition>,
    pub functions: IndexMap<Name, FunctionId, Function>,
    pub entrypoint: Function,
}

entity_id!(InstructionId, u32);

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Parameter(Parameter),
    Reference(Reference),
    Apply(Apply),
    Binary(Binary),
    Unary(Unary),
    Integer(Integer),
    Number(Number),
    True,
    False,
}

const _: () = assert!(std::mem::size_of::<Instruction>() <= 16, "Instruction size cannot be over 16 bytes");

entity_id!(InstructionListId, u32);

#[derive(Debug, Clone)]
pub struct InstructionList {
    pub instructions: Vec<InstructionId>,
}

entity_id!(BlockId, u32);

#[derive(Debug, Clone)]
pub struct Block {
    pub instructions: InstructionListId,
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
pub enum Reference {
    Constant(DefinitionId),
    Function(FunctionId),
}

impl Instruction {
    pub const fn constant_ref(definition_id: DefinitionId) -> Instruction {
        Instruction::Reference(Reference::Constant(definition_id))
    }

    pub const fn function_ref(function_id: FunctionId) -> Instruction {
        Instruction::Reference(Reference::Function(function_id))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Apply {
    pub callee: InstructionId,
    pub arguments: InstructionListId,
}

impl Instruction {
    pub const fn apply(callee: InstructionId, arguments: InstructionListId) -> Instruction {
        Instruction::Apply(Apply {
            callee,
            arguments,
        })
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
    pub blocks: Vec<BlockId>,
}

entity_id!(FunctionId, u32);

#[derive(Debug)]
pub struct Function {
    pub blocks: Vec<BlockId>,
}

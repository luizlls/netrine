use crate::collections::IndexMap;
use crate::collections::IndexVec;
use crate::hir;
use crate::interner::Name;
use crate::macros::entity_id;
use crate::types;
use crate::types::TypeId;

#[derive(Debug)]
pub struct Module {
    pub definitions: IndexMap<Name, DefinitionId, Definition>,
    pub functions: IndexMap<Name, FunctionId, Function>,
    pub entrypoint: Function,
}

entity_id!(DefinitionId, u32);

#[derive(Debug)]
pub struct Definition {
    pub instructions: IndexVec<InstructionId, Instruction>,
}

entity_id!(FunctionId, u32);

#[derive(Debug)]
pub struct Function {
    pub instructions: IndexVec<InstructionId, Instruction>,
}

entity_id!(InstructionId, u32);

#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub type_id: TypeId,
}

impl Instruction {
    pub fn new(kind: InstructionKind, type_id: TypeId) -> Instruction {
        Instruction { kind, type_id }
    }
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
    Parameter(Parameter),
    Reference(Reference),
    Apply(Apply),
    Binary(Binary),
    Unary(Unary),
    Integer(Integer),
    Number(Number),
    ToNumber(ToNumber),
    True,
    False,
}

#[derive(Debug, Clone, Copy)]
pub struct Parameter {
    pub position: u8,
}

impl Instruction {
    pub const fn parameter(position: u8, type_id: TypeId) -> Instruction {
        Instruction {
            kind: InstructionKind::Parameter(Parameter { position }),
            type_id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Reference {
    Definition(DefinitionId),
    Function(FunctionId),
}

impl Instruction {
    pub const fn definition_ref(definition_id: DefinitionId, type_id: TypeId) -> Instruction {
        Instruction {
            kind: InstructionKind::Reference(Reference::Definition(definition_id)),
            type_id,
        }
    }

    pub const fn function_ref(function_id: FunctionId, type_id: TypeId) -> Instruction {
        Instruction {
            kind: InstructionKind::Reference(Reference::Function(function_id)),
            type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub callee: InstructionId,
    pub arguments: Vec<InstructionId>,
}

impl Instruction {
    pub const fn apply(callee: InstructionId, arguments: Vec<InstructionId>, type_id: TypeId) -> Instruction {
        Instruction {
            kind: InstructionKind::Apply(Apply {
                callee,
                arguments,
            }),
            type_id,
        }
    }
}

pub type Operator = hir::Operator;

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub operand: InstructionId,
}

impl Instruction {
    pub const fn unary(operator: Operator, operand: InstructionId, type_id: TypeId) -> Instruction {
        Instruction {
            kind: InstructionKind::Unary(Unary {
                operator,
                operand,
            }),
            type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub loperand: InstructionId,
    pub roperand: InstructionId,
}

impl Instruction {
    pub const fn binary(
        operator: Operator,
        loperand: InstructionId,
        roperand: InstructionId,
        type_id: TypeId,
    ) -> Instruction {
        Instruction {
            kind: InstructionKind::Binary(Binary {
                operator,
                loperand,
                roperand,
            }),
            type_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Instruction {
    pub const fn integer(value: i64) -> Instruction {
        Instruction {
            kind: InstructionKind::Integer(Integer { value }),
            type_id: types::INTEGER,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Number {
    pub value: f64,
}

impl Instruction {
    pub const fn number(value: f64) -> Instruction {
        Instruction {
            kind: InstructionKind::Number(Number { value }),
            type_id: types::NUMBER,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ToNumber {
    pub source: InstructionId,
}

impl Instruction {
    pub const fn to_number(source: InstructionId) -> Instruction {
        Instruction {
            kind: InstructionKind::ToNumber(ToNumber { source }),
            type_id: types::NUMBER,
        }
    }
}

impl Instruction {
    pub const fn boolean(truthy: bool) -> Instruction {
        Instruction {
            kind: if truthy {
                InstructionKind::True
            } else {
                InstructionKind::False
            },
            type_id: types::BOOLEAN,
        }
    }
}

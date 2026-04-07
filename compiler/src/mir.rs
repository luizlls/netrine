use crate::collections::{IndexMap, IndexVec};
use crate::error::Result;
use crate::interner::{Interner, Name};
use crate::macros::entity_id;
use crate::source::{Source, Span};
use crate::syntax::{self, Syntax};
use crate::types::{self, TypeId};

#[derive(Debug)]
pub struct Module {
    globals: IndexMap<Name, GlobalId, Global>,
    functions: IndexMap<Name, FunctionId, Function>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            globals: IndexMap::new(),
            functions: IndexMap::new(),
        }
    }
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
    LoadGlobal(LoadGlobal),
    Binary(Binary),
    Unary(Unary),
    Integer(Integer),
    Number(Number),
    True,
    False,
}

#[derive(Debug, Clone)]
pub struct LoadGlobal {
    pub global: GlobalId,
}

impl Instruction {
    const fn load_global(global: GlobalId, type_id: TypeId) -> Instruction {
        Instruction {
            kind: InstructionKind::LoadGlobal(LoadGlobal { global }),
            type_id,
        }
    }
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

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub operand: InstructionId,
}

impl Instruction {
    const fn unary(operator: Operator, operand: InstructionId, type_id: TypeId) -> Instruction {
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
    const fn binary(
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
    const fn integer(value: i64) -> Instruction {
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
    const fn number(value: f64) -> Instruction {
        Instruction {
            kind: InstructionKind::Number(Number { value }),
            type_id: types::NUMBER,
        }
    }
}

impl Instruction {
    const fn boolean(truthy: bool) -> Instruction {
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

entity_id!(GlobalId, u32);

#[derive(Debug)]
pub struct Global {
    pub function: FunctionId,
    pub type_id: TypeId,
}

entity_id!(LocalId, u32);

#[derive(Debug)]
pub struct Local {
    pub type_id: TypeId,
}

entity_id!(FunctionId, u32);

#[derive(Debug)]
pub struct Function {
    pub instructions: IndexVec<InstructionId, Instruction>,
    pub types: IndexVec<InstructionId, TypeId>,
    pub spans: IndexVec<InstructionId, Span>,
    pub locals: IndexVec<LocalId, Local>,
    pub parameters: u8,
}

#[derive(Debug)]
pub struct FunctionBuilder {
    pub instructions: IndexVec<InstructionId, Instruction>,
    pub types: IndexVec<InstructionId, TypeId>,
    pub spans: IndexVec<InstructionId, Span>,
    pub locals: IndexMap<Name, LocalId, Local>,
    pub parameters: u8,
}

impl FunctionBuilder {
    fn new() -> FunctionBuilder {
        FunctionBuilder {
            instructions: IndexVec::new(),
            types: IndexVec::new(),
            spans: IndexVec::new(),
            locals: IndexMap::new(),
            parameters: 0,
        }
    }

    fn build(self) -> Function {
        Function {
            instructions: self.instructions,
            types: self.types,
            spans: self.spans,
            locals: self.locals.values(),
            parameters: self.parameters,
        }
    }
}

#[derive(Debug)]
struct LowerHir<'mir> {
    globals: IndexMap<Name, GlobalId, Global>,
    functions: IndexMap<Name, FunctionId, Function>,
    source: &'mir Source,
    interner: &'mir mut Interner,
    syntax: &'mir Syntax,
}

impl<'mir> LowerHir<'mir> {
    fn new(
        source: &'mir Source,
        interner: &'mir mut Interner,
        syntax: &'mir Syntax,
    ) -> LowerHir<'mir> {
        LowerHir {
            globals: IndexMap::new(),
            functions: IndexMap::new(),
            source,
            interner,
            syntax,
        }
    }

    fn lower(&mut self, nodes: &[syntax::Node], builder: FunctionBuilder) -> Result<Function> {
        for node in nodes {}
        Ok(builder.build())
    }
}

pub fn from_syntax(syntax: &Syntax, source: &Source, interner: &mut Interner) -> Result<Module> {
    let mut lower = LowerHir::new(source, interner, syntax);

    Ok(Module {
        globals: IndexMap::new(),
        functions: IndexMap::new(),
    })
}

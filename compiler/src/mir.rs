use crate::collections::{IndexMap, IndexVec};
use crate::error::Result;
use crate::interner::{Interner, Name};
use crate::macros::entity_id;
use crate::types::TypeId;
use crate::{hir, types};

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
    ToNumber(ToNumber),
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

pub type Operator = hir::Operator;

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

#[derive(Debug, Clone)]
pub struct ToNumber {
    pub source: InstructionId,
}

impl Instruction {
    const fn to_number(source: InstructionId) -> Instruction {
        Instruction {
            kind: InstructionKind::ToNumber(ToNumber { source }),
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
}

impl Function {
    fn new() -> Function {
        Function {
            instructions: IndexVec::new(),
        }
    }

    fn instruction(&mut self, instruction: Instruction) -> InstructionId {
        self.instructions.push(instruction)
    }
}

#[derive(Debug)]
struct Context {
    functions: Vec<Function>,
}

impl Context {
    fn new() -> Context {
        Context {
            functions: Vec::new(),
        }
    }

    fn push_function(&mut self, function: Function) {
        self.functions.push(function);
    }

    fn pop_function(&mut self) -> Function {
        self.functions.pop().expect("at least one function in the context")
    }

    fn function(&mut self) -> &mut Function {
        self.functions.last_mut().expect("at least one function in the context")
    }
}

#[derive(Debug)]
struct LowerHir<'mir> {
    globals: IndexMap<Name, GlobalId, Global>,
    functions: IndexMap<Name, FunctionId, Function>,
    context: Context,
    interner: &'mir mut Interner,
}

impl<'mir> LowerHir<'mir> {
    fn new(interner: &'mir mut Interner) -> LowerHir<'mir> {
        LowerHir {
            globals: IndexMap::new(),
            functions: IndexMap::new(),
            context: Context::new(),
            interner,
        }
        .init()
    }

    fn init(mut self) -> LowerHir<'mir> {
        self.context.push_function(Function::new());
        self
    }

    #[inline]
    fn instruction(&mut self, instruction: Instruction) -> InstructionId {
        self.context.function().instruction(instruction)
    }

    fn lower(&mut self, node: &hir::Node) -> Result<InstructionId> {
        match &node.kind {
            hir::NodeKind::Global(global) => self.global(node, global),
            hir::NodeKind::Unary(unary) => self.unary(node, unary),
            hir::NodeKind::Binary(binary) => self.binary(node, binary),
            hir::NodeKind::LocalRef(local) => self.local_ref(node, local),
            hir::NodeKind::GlobalRef(global) => self.global_ref(node, global),
            hir::NodeKind::Number(number) => self.number(node, number),
            hir::NodeKind::Integer(integer) => self.integer(node, integer),
            hir::NodeKind::True => self.boolean(node, true),
            hir::NodeKind::False => self.boolean(node, false),
        }
    }

    fn global(&mut self, node: &hir::Node, global: &hir::Global) -> Result<InstructionId> {
        self.context.push_function(Function::new());
        let value = self.lower(&global.value)?;

        let initializer = self.context.pop_function();
        let function = self.functions.insert(global.name, initializer);

        self.globals.insert(
            global.name,
            Global {
                function,
                type_id: node.type_id,
            },
        );

        Ok(value)
    }

    fn local_ref(&mut self, node: &hir::Node, local: &hir::LocalRef) -> Result<InstructionId> {
        todo!()
    }

    fn global_ref(&mut self, node: &hir::Node, global: &hir::GlobalRef) -> Result<InstructionId> {
        let id = self.globals.id(global.name).expect("malformed hir node");

        Ok(self.instruction(Instruction::load_global(id, node.type_id)))
    }

    fn number(&mut self, _node: &hir::Node, number: &hir::Number) -> Result<InstructionId> {
        Ok(self.instruction(Instruction::number(number.value)))
    }

    fn integer(&mut self, _node: &hir::Node, integer: &hir::Integer) -> Result<InstructionId> {
        Ok(self.instruction(Instruction::integer(integer.value)))
    }

    fn boolean(&mut self, _node: &hir::Node, truthy: bool) -> Result<InstructionId> {
        Ok(self.instruction(Instruction::boolean(truthy)))
    }

    fn coerce(
        &mut self,
        source: InstructionId,
        node_type: TypeId,
        other_type: TypeId,
        result_type: TypeId,
    ) -> InstructionId {
        if (other_type == types::NUMBER || result_type == types::NUMBER)
            && node_type == types::INTEGER
        {
            self.instruction(Instruction::to_number(source))
        } else {
            source
        }
    }

    fn unary(&mut self, node: &hir::Node, unary: &hir::Unary) -> Result<InstructionId> {
        let operand = self.lower(&unary.operand)?;
        Ok(self.instruction(Instruction::unary(unary.operator, operand, node.type_id)))
    }

    fn binary(&mut self, node: &hir::Node, binary: &hir::Binary) -> Result<InstructionId> {
        let loperand = self.lower(&binary.loperand)?;
        let roperand = self.lower(&binary.roperand)?;
        let loperand =
            self.coerce(loperand, binary.loperand.type_id, binary.roperand.type_id, node.type_id);
        let roperand =
            self.coerce(roperand, binary.roperand.type_id, binary.loperand.type_id, node.type_id);

        Ok(self.instruction(Instruction::binary(binary.operator, loperand, roperand, node.type_id)))
    }

    fn finish(mut self) -> Module {
        let entrypoint_symbol = self.interner.intern("__entrypoint");
        let entrypoint_function = self.context.pop_function();

        debug_assert!(self.context.functions.is_empty(), "function context is not empty");

        self.functions.insert(entrypoint_symbol, entrypoint_function);

        Module {
            globals: self.globals,
            functions: self.functions,
        }
    }
}

pub fn from_hir(module: &hir::Module, interner: &mut Interner) -> Result<Module> {
    let mut lower = LowerHir::new(interner);

    for node in &module.nodes {
        lower.lower(node)?;
    }

    Ok(lower.finish())
}

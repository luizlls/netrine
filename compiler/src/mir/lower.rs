use std::mem;

use hashbrown::HashMap;

use super::ir::*;
use crate::collections::{IndexMap, IndexVec};
use crate::error::Result;
use crate::hir;
use crate::interner::{Interner, Name};
use crate::types::{self, TypeId};

#[derive(Debug)]
struct Builder {
    kind: BuilderKind,
    instructions: IndexVec<InstructionId, Instruction>,
    locals: HashMap<Name, InstructionId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BuilderKind {
    Definition,
    Function,
}

impl Builder {
    fn new(kind: BuilderKind) -> Builder {
        Builder {
            kind,
            instructions: IndexVec::new(),
            locals: HashMap::new(),
        }
    }

    fn emit(&mut self, instruction: Instruction) -> InstructionId {
        self.instructions.push(instruction)
    }

    fn insert(&mut self, name: Name, instruction: InstructionId) {
        self.locals.insert(name, instruction);
    }

    fn build_function(self) -> Function {
        debug_assert!(self.kind == BuilderKind::Function);

        Function {
            instructions: self.instructions,
        }
    }

    fn build_defintion(self) -> Definition {
        debug_assert!(self.kind == BuilderKind::Definition);

        Definition {
            instructions: self.instructions,
        }
    }
}

#[derive(Debug)]
struct Context<'ctx> {
    definitions: IndexMap<Name, DefinitionId, Definition>,
    functions: IndexMap<Name, FunctionId, Function>,
    builder: Builder,
    builders: Vec<Builder>,
    interner: &'ctx Interner,
}

impl<'ctx> Context<'ctx> {
    fn new(interner: &'ctx Interner) -> Context<'ctx> {
        Context {
            definitions: IndexMap::new(),
            functions: IndexMap::new(),
            builder: Builder::new(BuilderKind::Function),
            builders: Vec::new(),
            interner,
        }
    }

    fn top_level(&self) -> bool {
        self.builders.is_empty()
    }

    fn setup_builder(&mut self, kind: BuilderKind) {
        let builder = mem::replace(&mut self.builder, Builder::new(kind));
        self.builders.push(builder);
    }

    fn pop_builder(&mut self) -> Builder {
        mem::replace(
            &mut self.builder,
            self.builders.pop().unwrap_or_else(|| Builder::new(BuilderKind::Function)),
        )
    }

    fn lookup_name(&self, name: Name) -> Option<InstructionId> {
        if let Some(&instruction_id) = self.builder.locals.get(&name) {
            return Some(instruction_id);
        }

        self.builders.iter().rev().find_map(|builder| builder.locals.get(&name)).copied()
    }
}

fn lower(ctx: &mut Context, node: &hir::Node) -> InstructionId {
    match &node.kind {
        hir::NodeKind::Function(function) => lower_function(ctx, node, function),
        hir::NodeKind::Definition(definition) => lower_definition(ctx, node, definition),
        hir::NodeKind::Apply(apply) => lower_apply(ctx, node, apply),
        hir::NodeKind::Unary(unary) => lower_unary(ctx, node, unary),
        hir::NodeKind::Binary(binary) => lower_binary(ctx, node, binary),
        hir::NodeKind::Reference(reference) => lower_reference(ctx, node, reference),
        hir::NodeKind::Number(number) => lower_number(ctx, node, number),
        hir::NodeKind::Integer(integer) => lower_integer(ctx, node, integer),
        hir::NodeKind::True => lower_boolean(ctx, node, true),
        hir::NodeKind::False => lower_boolean(ctx, node, false),
    }
}

fn emit(ctx: &mut Context, instruction: Instruction) -> InstructionId {
    ctx.builder.emit(instruction)
}

fn lower_function(ctx: &mut Context, _node: &hir::Node, function: &hir::Function) -> InstructionId {
    ctx.setup_builder(BuilderKind::Function);

    for (position, parameter) in function.parameters.iter().enumerate() {
        let instruction_id = emit(ctx, Instruction::parameter(position as u8, parameter.type_id));
        ctx.builder.insert(parameter.name, instruction_id);
    }

    let instruction_id = lower(ctx, &function.value);
    let function_ = ctx.pop_builder().build_function();
    ctx.functions.insert(function.name, function_);

    instruction_id
}

fn lower_definition(
    ctx: &mut Context,
    _node: &hir::Node,
    definition: &hir::Definition,
) -> InstructionId {
    let top_level = ctx.top_level();

    if top_level {
        ctx.setup_builder(BuilderKind::Definition);
    }

    let instruction_id = lower(ctx, &definition.value);

    if top_level {
        let definition_ = ctx.pop_builder().build_defintion();
        ctx.definitions.insert(definition.name, definition_);
    } else {
        ctx.builder.insert(definition.name, instruction_id);
    }

    instruction_id
}

fn lower_reference(
    ctx: &mut Context,
    node: &hir::Node,
    reference: &hir::Reference,
) -> InstructionId {
    match reference.kind {
        hir::ReferenceKind::Function => {
            let function_id = ctx.functions.id(reference.name).expect("malformed hir");
            emit(ctx, Instruction::function_ref(function_id, node.type_id))
        }
        hir::ReferenceKind::Global => {
            let definition_id = ctx.definitions.id(reference.name).expect("malformed hir");
            emit(ctx, Instruction::definition_ref(definition_id, node.type_id))
        }
        hir::ReferenceKind::Local => ctx.lookup_name(reference.name).expect("malformed hir"),
    }
}

fn lower_apply(ctx: &mut Context, node: &hir::Node, apply: &hir::Apply) -> InstructionId {
    let callee = lower(ctx, &apply.callee);
    let arguments = apply.arguments.iter().map(|arg| lower(ctx, arg)).collect();

    emit(ctx, Instruction::apply(callee, arguments, node.type_id))
}

fn coerce(
    ctx: &mut Context,
    source: InstructionId,
    node_type: TypeId,
    other_type: TypeId,
    result_type: TypeId,
) -> InstructionId {
    let integer_source = node_type == types::INTEGER;
    let number_target = other_type == types::NUMBER || result_type == types::NUMBER;

    if integer_source && number_target {
        emit(ctx, Instruction::to_number(source))
    } else {
        source
    }
}

fn lower_unary(ctx: &mut Context, node: &hir::Node, unary: &hir::Unary) -> InstructionId {
    let operand = lower(ctx, &unary.operand);
    emit(ctx, Instruction::unary(unary.operator, operand, node.type_id))
}

fn lower_binary(ctx: &mut Context, node: &hir::Node, binary: &hir::Binary) -> InstructionId {
    let loperand = lower(ctx, &binary.loperand);
    let loperand =
        coerce(ctx, loperand, binary.loperand.type_id, binary.roperand.type_id, node.type_id);

    let roperand = lower(ctx, &binary.roperand);
    let roperand =
        coerce(ctx, roperand, binary.roperand.type_id, binary.loperand.type_id, node.type_id);

    emit(ctx, Instruction::binary(binary.operator, loperand, roperand, node.type_id))
}

fn lower_number(ctx: &mut Context, _node: &hir::Node, number: &hir::Number) -> InstructionId {
    emit(ctx, Instruction::number(number.value))
}

fn lower_integer(ctx: &mut Context, _node: &hir::Node, integer: &hir::Integer) -> InstructionId {
    emit(ctx, Instruction::integer(integer.value))
}

fn lower_boolean(ctx: &mut Context, _node: &hir::Node, truthy: bool) -> InstructionId {
    emit(ctx, Instruction::boolean(truthy))
}

pub fn lower_hir(hir: &hir::Module, interner: &Interner) -> Result<Module> {
    let mut context = Context::new(interner);

    for node in &hir.nodes {
        lower(&mut context, node);
    }

    Ok(Module {
        entrypoint: context.pop_builder().build_function(),
        definitions: context.definitions,
        functions: context.functions,
    })
}

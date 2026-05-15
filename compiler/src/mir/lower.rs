use std::mem;

use hashbrown::HashMap;

use super::ir::*;
use crate::collections::{IndexMap, IndexVec};
use crate::error::{Error, Result};
use crate::interner::{Interner, Name};
use crate::source::{Source, Span};
use crate::syntax;
use crate::types::{self, TypeId};

#[derive(Debug)]
struct Builder {
    kind: BuilderKind,
    instructions: Vec<InstructionId>,
    parameters: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BuilderKind {
    Let,
    Fn,
    List,
}

impl Builder {
    const fn new(kind: BuilderKind) -> Builder {
        Builder {
            kind,
            instructions: Vec::new(),
            parameters: 0,
        }
    }

    fn is(&self, kind: BuilderKind) -> bool {
        self.kind == kind
    }

    fn push_instruction(&mut self, instruction: InstructionId) {
        self.instructions.push(instruction);
    }

    fn next_parameter(&mut self) -> u8 {
        self.parameters += 1;
        self.parameters - 1
    }

    fn build_instruction_list(self) -> InstructionList {
        InstructionList {
            instructions: self.instructions,
        }
    }
}

#[derive(Debug)]
struct Scope {
    entries: HashMap<Name, InstructionId>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            entries: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct Context<'ctx> {
    definitions: IndexMap<Name, DefinitionId, Definition>,
    functions: IndexMap<Name, FunctionId, Function>,
    instructions: IndexVec<InstructionId, Instruction>,
    types: IndexVec<InstructionId, TypeId>,
    spans: IndexVec<InstructionId, Span>,
    instruction_lists: IndexVec<InstructionListId, InstructionList>,
    blocks: IndexVec<BlockId, Block>,
    builder: Builder,
    builders: Vec<Builder>,
    scopes: Vec<Scope>,
    name_stack: Vec<(Name, Span)>,
    value_stack: Vec<InstructionId>,
    interner: &'ctx mut Interner,
    source: &'ctx Source,
    syntax: &'ctx syntax::Module,
}

impl<'ctx> Context<'ctx> {
    fn new(
        source: &'ctx Source,
        interner: &'ctx mut Interner,
        syntax: &'ctx syntax::Module,
    ) -> Context<'ctx> {
        Context {
            instructions: IndexVec::new(),
            types: IndexVec::new(),
            spans: IndexVec::new(),
            instruction_lists: IndexVec::new(),
            blocks: IndexVec::new(),
            definitions: IndexMap::new(),
            functions: IndexMap::new(),
            value_stack: Vec::new(),
            name_stack: Vec::new(),
            builder: Builder::new(BuilderKind::Fn),
            builders: Vec::new(),
            scopes: vec![Scope::new()],
            source,
            interner,
            syntax,
        }
    }

    fn top_level(&self) -> bool {
        self.builders.is_empty()
    }

    fn push_name(&mut self, name: Name, span: Span) {
        self.name_stack.push((name, span));
    }

    fn pop_name(&mut self) -> (Name, Span) {
        self.name_stack.pop().expect("mir: expected name in the context stack")
    }

    fn push_value(&mut self, value: InstructionId) {
        self.value_stack.push(value);
    }

    fn pop_value(&mut self) -> InstructionId {
        self.value_stack.pop().expect("mir: expected value in the context stack")
    }

    fn lookup_name(&self, name: Name) -> Option<&InstructionId> {
        self.scopes.iter().find_map(|scope| scope.entries.get(&name))
    }

    fn insert_name(&mut self, name: Name, instruction: InstructionId) {
        let scope = self.scopes.last_mut().expect("mir: expected scope entry");
        scope.entries.insert(name, instruction);
    }

    fn emit(&mut self, instruction: Instruction, span: Span) -> InstructionId {
        let instruction_id = self.instructions.push(instruction);
        self.spans.insert(instruction_id, span);
        self.types.insert(instruction_id, types::UNKNOWN);
        self.builder.push_instruction(instruction_id);
        instruction_id
    }

    fn emit_push(&mut self, instruction: Instruction, span: Span) {
        let instruction_id = self.emit(instruction, span);
        self.push_value(instruction_id);
    }

    fn setup_builder(&mut self, kind: BuilderKind) {
        let builder = mem::replace(&mut self.builder, Builder::new(kind));
        self.builders.push(builder);
        self.scopes.push(Scope::new());
    }

    fn pop_builder(&mut self) -> Builder {
        mem::replace(&mut self.builder, self.builders.pop().unwrap_or_else(|| Builder::new(BuilderKind::Fn)))
    }

    fn build_block(&mut self) -> BlockId {
        let instructions = self.pop_builder().build_instruction_list();
        let instructions = self.instruction_lists.push(instructions);
        self.blocks.push(Block { instructions })
    }

    fn build_function(&mut self) -> Function {
        let block = self.build_block();
        Function {
            blocks: vec![block],
        }
    }

    fn build_definition(&mut self) -> Definition {
        let block = self.build_block();
        Definition {
            blocks: vec![block],
        }
    }

    fn finalize(mut self) -> Module {
        let entrypoint = self.build_function();

        Module {
            instructions: self.instructions,
            types: self.types,
            spans: self.spans,
            instruction_lists: self.instruction_lists,
            blocks: self.blocks,
            definitions: self.definitions,
            functions: self.functions,
            entrypoint,
        }
    }
}

fn lower(ctx: &mut Context) -> Result<()> {
    for node in &ctx.syntax.nodes {
        let span = ctx.syntax.spans[node.token];

        match node.kind {
            syntax::NodeKind::LetInit => setup_definition(ctx)?,
            syntax::NodeKind::LetEnd => finish_definition(ctx)?,
            syntax::NodeKind::FnInit => setup_function(ctx)?,
            syntax::NodeKind::FnEnd => finish_function(ctx)?,
            syntax::NodeKind::ParameterInit => setup_parameter(ctx)?,
            syntax::NodeKind::ParameterEnd => finish_parameter(ctx)?,
            syntax::NodeKind::Identifier => identifier(ctx, span)?,
            syntax::NodeKind::Name => name(ctx, span)?,
            syntax::NodeKind::ApplyInit => setup_apply(ctx, span)?,
            syntax::NodeKind::ApplyEnd => finish_apply(ctx, span)?,
            syntax::NodeKind::Unary(operator) => unary(ctx, span, operator)?,
            syntax::NodeKind::Binary(operator) => binary(ctx, span, operator)?,
            syntax::NodeKind::Integer => integer(ctx, span)?,
            syntax::NodeKind::Number => number(ctx, span)?,
            syntax::NodeKind::True => boolean(ctx, span, true)?,
            syntax::NodeKind::False => boolean(ctx, span, false)?,
            syntax::NodeKind::GroupInit => {}
            syntax::NodeKind::GroupEnd => {}
        }
    }

    Ok(())
}

fn name_available(ctx: &mut Context, name: Name, span: Span) -> Result<()> {
    let existing_def = ctx.definitions.contains(name);
    let existing_fn = ctx.functions.contains(name);

    if existing_def || existing_fn {
        let name = &ctx.interner[name];
        return fail(span, format!("The name `{name}` is defined multiple times"));
    }

    Ok(())
}

fn setup_definition(ctx: &mut Context) -> Result<()> {
    if ctx.top_level() {
        ctx.setup_builder(BuilderKind::Let);
    }
    Ok(())
}

fn global_definition(ctx: &mut Context) -> Result<()> {
    let (name, span) = ctx.pop_name();

    name_available(ctx, name, span)?;

    let definition = ctx.build_definition();
    let definition_id = ctx.definitions.insert(name, definition);
    let instruction_id = ctx.emit(Instruction::constant_ref(definition_id), span);

    ctx.insert_name(name, instruction_id);

    Ok(())
}

fn local_definition(ctx: &mut Context) -> Result<()> {
    let (name, _span) = ctx.pop_name();

    let value = ctx.pop_value();
    ctx.insert_name(name, value);

    Ok(())
}

fn finish_definition(ctx: &mut Context) -> Result<()> {
    if ctx.builder.is(BuilderKind::Let) {
        global_definition(ctx)
    } else {
        local_definition(ctx)
    }
}

fn setup_function(ctx: &mut Context) -> Result<()> {
    ctx.setup_builder(BuilderKind::Fn);
    Ok(())
}

fn finish_function(ctx: &mut Context) -> Result<()> {
    let (name, span) = ctx.pop_name();

    name_available(ctx, name, span)?;

    let function = ctx.build_function();
    ctx.functions.insert(name, function);

    Ok(())
}

fn setup_parameter(_ctx: &mut Context) -> Result<()> {
    // there's nothing to setup for now
    Ok(())
}

fn finish_parameter(ctx: &mut Context) -> Result<()> {
    let (name, span) = ctx.pop_name();

    if ctx.lookup_name(name).is_some() {
        let name = &ctx.interner[name];
        return fail(span, format!("parameter `{name}` is already defined"));
    }

    let parameter_pos = ctx.builder.next_parameter();
    let instruction_id = ctx.emit(Instruction::parameter(parameter_pos), span);
    ctx.insert_name(name, instruction_id);

    Ok(())
}

fn name(ctx: &mut Context, span: Span) -> Result<()> {
    let value = ctx.source.slice(span);
    let name = ctx.interner.intern(value);
    ctx.push_name(name, span);

    Ok(())
}

fn identifier(ctx: &mut Context, span: Span) -> Result<()> {
    let value = ctx.source.slice(span);
    let name = ctx.interner.intern(value);

    if let Some(&instruction) = ctx.lookup_name(name) {
        ctx.push_value(instruction);
    } else if let Some(definition) = ctx.definitions.id(name) {
        ctx.emit_push(Instruction::constant_ref(definition), span);
    } else if let Some(function) = ctx.functions.id(name) {
        ctx.emit_push(Instruction::function_ref(function), span);
    } else {
        return fail(span, format!("value `{value}` not found"));
    }

    Ok(())
}

fn setup_apply(ctx: &mut Context, _span: Span) -> Result<()> {
    ctx.setup_builder(BuilderKind::List);
    Ok(())
}

fn finish_apply(ctx: &mut Context, span: Span) -> Result<()> {
    let arguments = ctx.pop_builder().build_instruction_list();
    Ok(())
}

fn unary(ctx: &mut Context, span: Span, operator: syntax::Operator) -> Result<()> {
    let operand = ctx.pop_value();

    let operator = match operator {
        syntax::Operator::Pos => Operator::Pos,
        syntax::Operator::Neg => Operator::Neg,
        syntax::Operator::Not => Operator::Not,
        _ => {
            return fail(span, "unsupported unary operator");
        }
    };

    ctx.emit_push(Instruction::unary(operator, operand), span);

    Ok(())
}

fn binary(ctx: &mut Context, span: Span, operator: syntax::Operator) -> Result<()> {
    let roperand = ctx.pop_value();
    let loperand = ctx.pop_value();

    let operator = match operator {
        syntax::Operator::Add => Operator::Add,
        syntax::Operator::Sub => Operator::Sub,
        syntax::Operator::Mul => Operator::Mul,
        syntax::Operator::Div => Operator::Div,
        syntax::Operator::Mod => Operator::Mod,
        syntax::Operator::Pow => Operator::Pow,
        syntax::Operator::Eq => Operator::Eq,
        syntax::Operator::Ne => Operator::Ne,
        syntax::Operator::Lt => Operator::Lt,
        syntax::Operator::Le => Operator::Le,
        syntax::Operator::Gt => Operator::Gt,
        syntax::Operator::Ge => Operator::Ge,
        syntax::Operator::And => Operator::And,
        syntax::Operator::Or => Operator::Or,
        _ => {
            return fail(span, "unsupported binary operator");
        }
    };

    ctx.emit_push(Instruction::binary(operator, loperand, roperand), span);

    Ok(())
}

fn integer(ctx: &mut Context, span: Span) -> Result<()> {
    let value = ctx.source.slice(span);

    let value = match &value.get(0..2) {
        Some("0b") => i64::from_str_radix(&value[2..], 2),
        Some("0x") => i64::from_str_radix(&value[2..], 16),
        _ => str::parse(value),
    };

    let Ok(value) = value else {
        return fail(span, "value is not supported as an integer");
    };

    ctx.emit_push(Instruction::integer(value), span);

    Ok(())
}

fn number(ctx: &mut Context, span: Span) -> Result<()> {
    let value = ctx.source.slice(span);

    let Ok(value) = str::parse(value) else {
        return fail(span, "value is not supported as an number");
    };

    ctx.emit_push(Instruction::number(value), span);

    Ok(())
}

fn boolean(ctx: &mut Context, span: Span, truthy: bool) -> Result<()> {
    ctx.emit_push(
        if truthy {
            Instruction::True
        } else {
            Instruction::False
        },
        span,
    );

    Ok(())
}

fn fail<T>(span: Span, message: impl Into<String>) -> Result<T> {
    Err(Error::error(span, message.into()))
}

pub fn lower_syntax(source: &Source, interner: &mut Interner, syntax: &syntax::Module) -> Result<Module> {
    let mut context = Context::new(source, interner, syntax);
    lower(&mut context)?;

    Ok(context.finalize())
}

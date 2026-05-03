use std::mem;

use hashbrown::HashMap;

use super::ir::*;
use crate::collections::IndexVec;
use crate::interner::Name;
use crate::source::{Span, WithSpan};
use crate::types::{self, TypeId};

#[derive(Debug)]
pub struct Builder {
    kind: BuilderKind,
    instructions: IndexVec<InstructionId, Instruction>,
    types: IndexVec<InstructionId, TypeId>,
    spans: IndexVec<InstructionId, Span>,
    parameters: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuilderKind {
    Let,
    Fn,
}

impl Builder {
    pub fn new(kind: BuilderKind) -> Builder {
        Builder {
            kind,
            instructions: IndexVec::new(),
            types: IndexVec::new(),
            spans: IndexVec::new(),
            parameters: 0,
        }
    }

    pub fn emit(&mut self, instruction: Instruction, span: Span) -> InstructionId {
        let id = self.instructions.push(instruction);
        self.spans.insert(id, span);
        self.types.insert(id, types::UNKNOWN);
        id
    }

    pub fn next_parameter(&mut self) -> u8 {
        self.parameters += 1;
        self.parameters - 1
    }

    pub fn build_function(self) -> Function {
        Function {
            instructions: self.instructions,
            types: self.types,
            spans: self.spans,
        }
    }

    pub fn build_definition(self) -> Definition {
        Definition {
            instructions: self.instructions,
            types: self.types,
            spans: self.spans,
        }
    }

    pub fn is(&self, kind: BuilderKind) -> bool {
        self.kind == kind
    }
}

#[derive(Debug)]
pub struct Scope {
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
pub struct Context {
    values: Vec<InstructionId>,
    names: Vec<WithSpan<Name>>,
    builder: Builder,
    builders: Vec<Builder>,
    scopes: Vec<Scope>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            values: Vec::new(),
            names: Vec::new(),
            builder: Builder::new(BuilderKind::Fn),
            builders: Vec::new(),
            scopes: vec![Scope::new()],
        }
    }

    pub fn top_level(&self) -> bool {
        self.builders.is_empty()
    }

    pub fn push_name(&mut self, name: Name, span: Span) {
        self.names.push(WithSpan::new(name, span));
    }

    pub fn pop_name(&mut self) -> WithSpan<Name> {
        self.names.pop().expect("mir: expected name in the context stack")
    }

    pub fn push_value(&mut self, value: InstructionId) {
        self.values.push(value);
    }

    pub fn pop_value(&mut self) -> InstructionId {
        self.values.pop().expect("mir: expected value in the context stack")
    }

    pub fn lookup_name(&self, name: Name) -> Option<&InstructionId> {
        self.scopes.iter().find_map(|scope| scope.entries.get(&name))
    }

    pub fn insert_name(&mut self, name: Name, instruction: InstructionId) {
        let scope = self.scopes.last_mut().expect("mir: expected scope entry");
        scope.entries.insert(name, instruction);
    }

    pub fn emit(&mut self, instruction: Instruction, span: Span) -> InstructionId {
        let instruction_id = self.builder.emit(instruction, span);
        self.push_value(instruction_id);
        instruction_id
    }

    pub fn setup_builder(&mut self, kind: BuilderKind) {
        let builder = mem::replace(&mut self.builder, Builder::new(kind));
        self.builders.push(builder);
        self.scopes.push(Scope::new());
    }

    pub fn pop_builder(&mut self) -> Builder {
        mem::replace(&mut self.builder, self.builders.pop().unwrap_or_else(|| Builder::new(BuilderKind::Fn)))
    }

    #[inline]
    pub fn builder(&mut self) -> &mut Builder {
        &mut self.builder
    }

    pub fn finalize(mut self) -> Function {
        self.pop_builder().build_function()
    }
}

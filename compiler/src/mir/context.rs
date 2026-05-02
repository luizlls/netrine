use std::mem;

use super::ir::*;
use crate::collections::IndexVec;
use crate::interner::Name;
use crate::source::{Span, WithSpan};
use crate::types::{self, TypeId};

#[derive(Debug)]
pub struct Builder {
    instructions: IndexVec<InstructionId, Instruction>,
    types: IndexVec<InstructionId, TypeId>,
    spans: IndexVec<InstructionId, Span>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            instructions: IndexVec::new(),
            types: IndexVec::new(),
            spans: IndexVec::new(),
        }
    }

    pub fn emit(&mut self, instruction: Instruction, span: Span) -> InstructionId {
        let id = self.instructions.push(instruction);
        self.spans.insert(id, span);
        self.types.insert(id, types::UNKNOWN);
        id
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
}

#[derive(Debug)]
pub struct Context {
    values: Vec<InstructionId>,
    names: Vec<WithSpan<Name>>,
    current: Builder,
    builders: Vec<Builder>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            values: Vec::new(),
            names: Vec::new(),
            current: Builder::new(),
            builders: Vec::new(),
        }
    }

    pub fn at_top_level(&self) -> bool {
        self.builders.is_empty()
    }

    pub fn push_name(&mut self, name: Name, span: Span) {
        self.names.push(WithSpan::new(name, span));
    }

    pub fn pop_name(&mut self) -> WithSpan<Name> {
        self.names.pop().expect("mir: missing name in the context stack")
    }

    pub fn push_value(&mut self, value: InstructionId) {
        self.values.push(value);
    }

    pub fn pop_value(&mut self) -> InstructionId {
        self.values.pop().expect("mir: missing value in the context stack")
    }

    pub fn emit(&mut self, instruction: Instruction, span: Span) -> InstructionId {
        let instruction_id = self.current.emit(instruction, span);
        self.push_value(instruction_id);
        instruction_id
    }

    pub fn push_builder(&mut self) {
        let current = mem::replace(&mut self.current, Builder::new());
        self.builders.push(current);
    }

    pub fn pop_builder(&mut self) -> Builder {
        mem::replace(&mut self.current, self.builders.pop().unwrap_or_else(Builder::new))
    }
}

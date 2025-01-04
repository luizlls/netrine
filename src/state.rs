use crate::source::{Source, SourceId};
use crate::types::{self, Type, TypeId};

#[derive(Debug)]
pub struct State {
    pub types: Vec<Type>,
    pub sources: Vec<Source>,
}

impl State {
    pub fn new() -> State {
        State {
            types: vec![],
            sources: vec![],
        }
        .init()
    }

    fn init(mut self) -> State {
        self.types.extend(Type::builtin());
        self
    }

    pub fn source(&self, source_id: SourceId) -> &Source {
        &self.sources[source_id.index()]
    }
}

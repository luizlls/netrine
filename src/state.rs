use crate::types::{self, Type, TypeId};

#[derive(Debug)]
pub struct State {
    pub types: Vec<Type>,
}

impl State {
    pub fn new() -> State {
        State {
            types: vec![],
        }
        .init()
    }

    fn init(mut self) -> State {
        self.types.extend(Type::builtin());
        self
    }
}

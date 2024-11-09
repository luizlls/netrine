use std::collections::HashMap;

use crate::ir::*;
use crate::types::{self, Type, TypeId};

pub struct Database {
    functions: HashMap<String, FunctionId>,
    constants: HashMap<String, ConstantId>,
}

pub struct State {
    functions: Vec<Function>,
    constants: Vec<Constant>,
}

impl State {
    pub fn new() -> State {
        State {
            functions: vec![],
            constants: vec![],
        }
    }
}

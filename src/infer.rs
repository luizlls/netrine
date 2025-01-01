use crate::error::{error, Error, Result};
use crate::syntax;
use crate::types::{Type, TypeId};

pub struct Infer {}

impl Infer {
    pub fn new() -> Infer {
        Infer {}
    }
}

pub fn infer(syntax: &[syntax::Node]) {
    let mut infer = Infer::new();

    for node in syntax {
        infer_node(&mut infer, node);
    }
}

fn infer_node(i: &mut Infer, syntax: &syntax::Node) {
    todo!()
}

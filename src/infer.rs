use crate::node::{Module, Node};
use crate::types::{Type, TypeId};

pub struct Infer {}

impl Infer {
    pub fn new() -> Infer {
        Infer {}
    }
}

pub fn infer(module: &Module) {
    let mut infer = Infer::new();

    for node in &module.nodes {
        infer_node(&mut infer, node);
    }
}

fn infer_node(l: &mut Infer, node: &Node) {
    todo!()
}

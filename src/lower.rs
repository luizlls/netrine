use crate::ir::*;
use crate::node::{Module, Node};

struct Lower {
    blocks: Vec<Block>,
    block: BlockId,
}

impl Lower {
    fn new() -> Lower {
        let block_id = BlockId(0);

        Lower {
            blocks: vec![
                Block::new(block_id),
            ],
            block: block_id,
        }
    }
}

pub fn lower(module: &Module) {
    let mut lower = Lower::new();

    for node in &module.nodes {
        lower_node(&mut lower, node);
    }
}

fn lower_node(l: &mut Lower, node: &Node) {
    todo!()
}

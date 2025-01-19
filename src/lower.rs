use crate::ir::*;
use crate::ast::Node;

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

pub fn lower(node: &Node) {
}

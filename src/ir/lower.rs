use crate::syntax;
use crate::error::{error, Error, Result};

use super::node::*;

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

pub fn lower(syntax: &[syntax::Node]) {
    let mut lower = Lower::new();

    for node in syntax {
        lower_node(&mut lower, node);
    }
}

fn lower_node(l: &mut Lower, node: &syntax::Node) {
    todo!()
}

use crate::syntax;
use crate::error::{error, Error, Result};

use super::node::*;

struct Lower {
    blocks: Vec<Block>,
    block: BlockId,
}

pub fn lower(syntax: &[syntax::Node]) -> Result<Vec<Block>> {
    todo!()
}

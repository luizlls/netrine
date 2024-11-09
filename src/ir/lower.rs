use crate::syntax;
use crate::error::{error, Error, Result};

use super::node::*;

struct Lower<'l> {
    blocks: Vec<Block>,
    block: BlockId,
    syntax: &'l [syntax::Node]
}

pub fn lower(syntax: &[syntax::Node]) -> Result<Vec<Function>> {
    todo!()
}

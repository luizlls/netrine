mod node;
mod lower;

pub use node::*;

use crate::error::Result;
use crate::syntax;

pub fn lower(nodes: &[syntax::Node]) -> Result<Vec<Block>> {
    let blocks = nodes.iter().map(lower::lower).collect();
    blocks
}

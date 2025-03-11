mod lower;
mod node;

pub use node::*;

use crate::error::Result;
use crate::syntax;

pub fn lower(nodes: &[syntax::Node]) -> Result<Vec<Block>> {
    nodes.iter().map(lower::lower).collect()
}

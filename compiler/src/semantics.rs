mod lower;
mod node;
mod types;

pub use types::Type;
pub use node::*;

use crate::error::Result;
use crate::syntax;

pub fn lower(module: &syntax::Module) -> Result<Module> {
    let nodes = lower::lower(module)?;
    Ok(Module {
        nodes,
    })
}

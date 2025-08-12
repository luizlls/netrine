mod lower;
mod node;

pub use node::*;

use crate::error::Result;
use crate::semantics;

pub fn lower(module: &semantics::Module) -> Result<Module> {
    let instructions = lower::lower(module)?;
    Ok(Module {
        instructions,
    })
}

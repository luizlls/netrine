mod lower;
mod node;

pub use node::*;

use crate::error::Result;
use crate::syntax;

pub fn lower(module: &syntax::Module) -> Result<Module> {
    let instructions = lower::lower(module)?;
    Ok(Module {
        functions: vec![
            Function {
                instructions,
            }
        ]
    })
}

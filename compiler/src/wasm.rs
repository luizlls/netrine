use crate::error::Result;
use crate::mir;

mod codegen;

pub fn compile(module: &mir::Module) -> Result<Vec<u8>> {
    Ok(codegen::compile(module))
}

mod lexer;
mod node;
mod parser;
mod token;

pub use node::*;

use crate::error::Result;
use crate::source::Source;

pub fn parse(source: &Source) -> Result<Module> {
    let tokens = lexer::tokens(source);
    let nodes = parser::parse(tokens)?;
    Ok(Module {
        nodes,
    })
}

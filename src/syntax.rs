mod lexer;
mod node;
mod parser;
mod token;

pub use node::*;

use crate::error::Result;
use crate::source::Source;

pub fn parse(source: &Source) -> Result<Vec<Node>> {
    let tokens = lexer::tokens(source);
    let nodes = parser::parse(source, &tokens);
    nodes
}

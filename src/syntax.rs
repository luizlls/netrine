use crate::error::Result;

mod lexer;
mod node;
mod parser;
mod token;

pub use node::*;

pub fn parse(source: &str) -> Result<Vec<Node>> {
    let tokens = lexer::tokens(&source);
    let module = parser::parse(&source, tokens);
    module
}

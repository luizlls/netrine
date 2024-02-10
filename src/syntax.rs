#![macro_use]

use crate::error::Result;

pub mod node;
mod lexer;
mod parser;
mod token;

pub fn parse(source: &str) -> Result<Vec<node::Node>> {
    let tokens = lexer::tokens(&source);
    let nodes = parser::parse(&source, &tokens).map_err(|err| err.into());
    nodes
}

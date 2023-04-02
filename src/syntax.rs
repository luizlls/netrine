#![macro_use]

use crate::source::Source;

mod lexer;
mod parser;
pub mod node;

pub fn parse(source: &Source) -> Vec<node::SyntaxNode> {
    parser::Parser::new(&source.content).parse()
}
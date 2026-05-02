mod lexer;
mod node;
mod parser;
mod token;

pub use node::*;

use crate::error::Result;
use crate::source::Source;

pub fn parse(source: &Source) -> Result<Syntax> {
    let tokens = lexer::tokens(source);
    let syntax = parser::parse(tokens);
    syntax
}

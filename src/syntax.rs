#![macro_use]

use crate::error::Result;
use crate::source::Source;

mod lexer;
mod token;
mod parser;
pub mod nodes;

pub fn parse(source: &Source) -> Result<nodes::Node> {
    parser::Parser::new(source).expr()
}

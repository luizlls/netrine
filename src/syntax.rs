#![macro_use]

use crate::error::Result;
use crate::source::Source;

use lexer::Lexer;
use node::Node;
use parser::Parser;

mod lexer;
mod parser;
mod token;
pub mod node;

pub fn parse(source: Source) -> Result<Vec<Node>> {
    let mut lexer = Lexer::new(&source.content);
    let mut parser = Parser::new(&source.content, lexer);
    parser::parse(&mut parser)
}

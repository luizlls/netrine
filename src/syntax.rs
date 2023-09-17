#![macro_use]

use crate::error::Result;
use crate::source::Source;

use lexer::Lexer;
use node::Node;
use parser::Parser;

pub mod node;
mod lexer;
mod parser;
mod token;

pub fn parse(source: &Source) -> Result<Vec<Node>> {
    let mut lexer = Lexer::new(&source.content);
    let tokens = lexer::tokenize(&mut lexer);
    let mut parser = Parser::new(&source.content, &tokens);
    let nodes = parser::parse(&mut parser);
    nodes
}

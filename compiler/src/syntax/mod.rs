mod lexer;
mod parser;
mod syntax;
mod token;

pub use syntax::*;

use crate::error::Result;
use crate::source::Source;

pub fn parse(source: &Source) -> Result<Module> {
    parser::parse(lexer::tokens(source))
}

#![macro_use]

use crate::source::Source;

mod lexer;
mod token;

pub fn parse(source: &Source) -> Vec<token::Token> {
    lexer::Lexer::new(&source.content).into_iter().collect()
}

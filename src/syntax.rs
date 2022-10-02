#![macro_use]

pub mod node;
mod lexer;
mod parser;
mod token;

pub use parser::parse;

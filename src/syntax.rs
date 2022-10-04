#![macro_use]

pub mod nodes;
mod lexer;
mod parser;
mod token;

pub use parser::parse;

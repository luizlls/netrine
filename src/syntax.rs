pub mod ast;
mod lexer;
mod parser;
mod token;

pub use parser::parse;

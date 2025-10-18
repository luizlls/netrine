mod compiler;
mod error;
mod index_map;
mod lexer;
mod mir;
mod parser;
mod pprint;
mod source;
mod state;
mod syntax;
mod token;
mod type_check;
mod types;
mod wasm;

pub use compiler::Compiler;

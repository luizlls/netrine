mod collections;
mod compiler;
mod error;
mod hir;
mod interner;
mod lexer;
mod macros;
mod mir;
mod parser;
mod source;
mod syntax;
mod token;
mod types;
// mod wasm;

pub use compiler::Compiler;
pub use error::Result;
pub use source::Source;

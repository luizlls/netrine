mod collections;
mod compiler;
mod error;
mod interner;
mod macros;
mod mir;
mod source;
mod syntax;
mod types;

pub use compiler::Compiler;
pub use error::Result;
pub use source::Source;

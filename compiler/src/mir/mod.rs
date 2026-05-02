mod context;
mod ir;
mod lower;

pub use ir::*;

use crate::error::Result;
use crate::interner::Interner;
use crate::source::Source;
use crate::syntax::Syntax;

pub fn from_syntax(source: &Source, interner: &mut Interner, syntax: &Syntax) -> Result<Module> {
    lower::lower_syntax(source, interner, syntax)
}

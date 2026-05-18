mod ir;
mod lower;

pub use ir::*;

use crate::error::Result;
use crate::interner::Interner;
use crate::source::Source;
use crate::syntax;

pub fn from_syntax(
    syntax: &syntax::Module,
    source: &Source,
    interner: &mut Interner,
) -> Result<Module> {
    lower::lower_syntax(syntax, source, interner)
}

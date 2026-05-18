mod ir;
mod lower;

pub use ir::*;

use crate::error::Result;
use crate::hir;
use crate::interner::Interner;

pub fn from_hir(hir: &hir::Module, interner: &Interner) -> Result<Module> {
    lower::lower_hir(hir, interner)
}

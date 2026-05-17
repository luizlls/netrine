use std::fs;

use crate::error::Result;
use crate::interner::Interner;
use crate::source::Source;
use crate::{hir, syntax};

pub struct Compiler<'state> {
    source: &'state Source,
    interner: Interner,
}

impl<'state> Compiler<'state> {
    pub fn new(source: &'state Source) -> Compiler<'state> {
        Compiler {
            source,
            interner: Interner::new(),
        }
    }

    pub fn parse(&mut self) -> Result<syntax::Module> {
        syntax::parse(&self.source)
    }

    pub fn hir(&mut self) -> Result<hir::Module> {
        let ast = self.parse()?;
        hir::from_syntax(&ast, &self.source, &mut self.interner)
    }

    // pub fn mir(&mut self) -> Result<mir::Module> {
    //     let hir = self.hir()?;
    //     mir::from_hir(&hir, &mut self.interner)
    // }

    pub fn compile(&mut self) -> Result<Vec<u8>> {
        // let mir = self.mir()?;
        // wasm::compile(&mir)
        Ok(vec![])
    }

    pub fn build(&mut self) -> Result<()> {
        let wasm = self.compile()?;
        fs::write("output.wasm", wasm).expect("Failed to write to output file");
        Ok(())
    }
}

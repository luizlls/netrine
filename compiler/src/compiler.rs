use std::fs;

use crate::error::Result;
use crate::interner::Interner;
use crate::mir;
use crate::source::Source;
use crate::syntax;

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

    pub fn parse(&mut self) -> Result<syntax::Syntax> {
        syntax::parse(self.source)
    }

    pub fn mir(&mut self) -> Result<mir::Module> {
        let syntax = self.parse()?;
        mir::from_syntax(self.source, &mut self.interner, &syntax)
    }

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

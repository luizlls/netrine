use std::fs;

use crate::error;
use crate::hir;
use crate::lexer;
use crate::mir;
use crate::parser;
use crate::source;
use crate::state;
use crate::syntax;
use crate::wasm;

use error::Result;

pub struct Compiler {
    state: state::State,
    source: source::Source,
}

impl Compiler {
    pub fn new(path: String, source: String) -> Compiler {
        Compiler {
            state: state::State::new(),
            source: source::Source::new(path, source),
        }
    }

    pub fn source(&self) -> &source::Source {
        &self.source
    }

    fn parse(&mut self) -> Result<syntax::Module> {
        let tokens = lexer::tokens(&self.source);
        parser::parse(tokens)
    }

    fn hir(&mut self) -> Result<hir::Module> {
        let ast = self.parse()?;
        hir::from_syntax(&ast, &mut self.state)
    }

    fn mir(&mut self) -> Result<mir::Module> {
        let hir = self.hir()?;
        mir::from_hir(&hir, &mut self.state)
    }

    pub fn compile(&mut self) -> Result<Vec<u8>> {
        let mir = self.mir()?;
        wasm::compile(&mir)
    }

    pub fn build(&mut self) -> Result<()> {
        let wasm = self.compile()?;
        fs::write("output.wasm".to_string(), wasm).expect("Failed to write to output file");
        Ok(())
    }
}

use crate::error;
use crate::eval;
use crate::hir;
use crate::lexer;
use crate::mir;
use crate::parser;
use crate::source;
use crate::state;
use crate::syntax;
use crate::wasm;

use error::Result;

pub struct Compiler<'c> {
    source: source::Source<'c>,
    state: state::State,
}

impl<'c> Compiler<'c> {
    pub fn from_source(file_path: String, source: &'c str) -> Compiler<'c> {
        Compiler {
            source: source::Source::new(file_path, source),
            state: state::State::new(),
        }
    }

    pub fn source(&self) -> &source::Source<'_> {
        &self.source
    }

    fn parse(&mut self) -> Result<syntax::Module> {
        let tokens = lexer::tokens(&self.source);
        parser::parse(tokens)
    }

    fn hir(&mut self) -> Result<hir::Module> {
        let syntax = self.parse()?;
        hir::from_syntax(&syntax, &mut self.state)
    }

    fn mir(&mut self) -> Result<mir::Module> {
        let hir = self.hir()?;
        mir::from_hir(&hir, &self.state)
    }

    pub fn compile(&mut self) -> Result<Vec<u8>> {
        let mir = self.mir()?;
        wasm::compile(&mir)
    }

    pub fn dump_ast(mut self) -> Result<String> {
        let ast = self.parse()?;
        Ok(format!("{ast}"))
    }

    pub fn dump_hir(mut self) -> Result<String> {
        let hir = self.hir()?;
        Ok(format!("{hir}"))
    }

    pub fn dump_mir(mut self) -> Result<String> {
        let mir = self.mir()?;
        Ok(format!("{mir}"))
    }

    pub fn eval(mut self) -> Result<String> {
        let mir = self.mir()?;
        Ok(eval::eval(&mir))
    }
}

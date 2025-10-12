use crate::error;
use crate::hir;
use crate::lexer;
use crate::mir;
use crate::parser;
use crate::pprint::PrettyPrint;
use crate::source;
use crate::state;
use crate::syntax;
use crate::type_check;
use crate::wasm;

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

    pub fn compile(&mut self) -> error::Result<Vec<u8>> {
        let mir = self.mir()?;
        wasm::compile(&mir)
    }

    pub fn dump_ast(mut self) -> error::Result<String> {
        let ast = self.parse()?;
        Ok(format!("{}", ast.pprint(&self.state)))
    }

    pub fn dump_hir(mut self) -> error::Result<String> {
        let hir = self.hir()?;
        Ok(format!("{}", hir.pprint(&self.state)))
    }

    pub fn dump_mir(mut self) -> error::Result<String> {
        let mir = self.mir()?;
        Ok(format!("{}", mir))
    }

    fn parse(&mut self) -> error::Result<syntax::Module> {
        let tokens = lexer::tokens(&self.source);
        parser::parse(tokens, &mut self.state)
    }

    fn hir(&mut self) -> error::Result<hir::Module> {
        let syntax = self.parse()?;
        hir::from_syntax(&syntax).and_then(type_check::check)
    }

    fn mir(&mut self) -> error::Result<mir::Module> {
        let hir = self.hir()?;
        mir::from_hir(&hir)
    }
}

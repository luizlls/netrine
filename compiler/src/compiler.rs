use crate::error;
use crate::lexer;
use crate::mir;
use crate::parser;
use crate::pprint::PrettyPrint;
use crate::source;
use crate::state;
use crate::syntax;
use crate::type_check;
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
        parser::parse(tokens, &mut self.state)
    }

    fn check(&self, syntax: &syntax::Module) -> Result<type_check::Types> {
        type_check::check(&syntax)
    }

    fn mir(&mut self) -> Result<mir::Module> {
        let syntax = self.parse()?;
        let types = self.check(&syntax)?;
        mir::from_syntax(&syntax, &types)
    }

    pub fn compile(&mut self) -> Result<Vec<u8>> {
        let mir = self.mir()?;
        wasm::compile(&mir)
    }

    pub fn dump_ast(mut self) -> Result<String> {
        let ast = self.parse()?;
        Ok(format!("{}", ast.pprint(&self.state)))
    }

    pub fn dump_mir(mut self) -> Result<String> {
        let mir = self.mir()?;
        Ok(format!("{}", mir.pprint(&self.state)))
    }
}

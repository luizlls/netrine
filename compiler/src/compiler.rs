use crate::error;
use crate::lexer;
use crate::mir;
use crate::parser;
use crate::pprint::PrettyPrint;
use crate::resolver;
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

    fn resolve(&mut self, syntax: &syntax::Module) -> Result<()> {
        resolver::resolve(syntax, &mut self.state)
    }

    fn check(&mut self, syntax: &syntax::Module) -> Result<()> {
        type_check::check(syntax, &mut self.state)
    }

    fn mir(&mut self) -> Result<mir::Module> {
        let syntax = self.parse()?;
        self.resolve(&syntax)?;
        self.check(&syntax)?;
        mir::from_syntax(&syntax, &self.state)
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

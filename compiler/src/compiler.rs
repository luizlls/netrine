use std::fs;

use crate::config;
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

pub struct Compiler {
    state: state::State,
    source: source::Source,
    config: config::Config,
}

impl Compiler {
    pub fn new(path: String, source: String, config: config::Config) -> Compiler {
        Compiler {
            state: state::State::new(),
            source: source::Source::new(path, source),
            config,
        }
    }

    pub fn source(&self) -> &source::Source {
        &self.source
    }

    fn parse(&mut self) -> Result<syntax::Module> {
        let source = &self.source;
        let tokens = lexer::tokens(source);
        let syntax = parser::parse(tokens)?;

        if self.config.dump_ast {
            println!("{syntax}")
        }

        Ok(syntax)
    }

    fn hir(&mut self) -> Result<hir::Module> {
        let ast = self.parse()?;
        let hir = hir::from_syntax(&ast, &mut self.state)?;

        if self.config.dump_hir {
            println!("{hir}")
        }

        Ok(hir)
    }

    fn mir(&mut self) -> Result<mir::Module> {
        let hir = self.hir()?;
        let mir = mir::from_hir(&hir, &mut self.state)?;

        if self.config.dump_mir {
            println!("{mir}")
        }

        Ok(mir)
    }

    pub fn eval(&mut self) -> Result<String> {
        let mir = self.mir()?;
        Ok(eval::eval(&mir))
    }

    pub fn compile(&mut self) -> Result<Vec<u8>> {
        let mir = self.mir()?;
        wasm::compile(&mir)
    }

    pub fn build(&mut self) -> Result<()> {
        let wasm = self.compile()?;
        fs::write(self.config.output.clone(), wasm).expect("Failed to write to output file");
        Ok(())
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
}

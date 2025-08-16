mod error;
mod source;
mod lexer;
mod token;
mod parser;
mod syntax;
mod hir;
mod mir;
mod types;
mod wasm;

pub fn source<'s>(path: String, content: &'s str) -> source::Source<'s> {
    source::Source::new(path, content)
}

pub fn parse(source: &source::Source) -> error::Result<syntax::Module> {
    let tokens = lexer::tokens(source);
    let syntax = parser::parse(tokens)?;
    Ok(syntax)
}

pub fn compile(source: &source::Source) -> error::Result<Vec<u8>> {
    let tokens = lexer::tokens(source);
    let syntax = parser::parse(tokens)?;
    let hir = hir::from_syntax(&syntax)?;
    let mir = mir::from_hir(&hir)?;
    let wasm = wasm::compile(&mir)?;
    Ok(wasm)
}

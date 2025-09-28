mod error;
mod hir;
mod lexer;
mod mir;
mod parser;
mod pprint;
mod source;
mod syntax;
mod token;
mod type_check;
mod types;
mod wasm;

pub fn source<'s>(path: String, content: &'s str) -> source::Source<'s> {
    source::Source::new(path, content)
}

pub fn compile(source: &source::Source) -> error::Result<Vec<u8>> {
    let tokens = lexer::tokens(source);
    let syntax = parser::parse(tokens)?;
    let mut hir = hir::from_syntax(&syntax)?;
    type_check::check(&mut hir)?;
    let mir = mir::from_hir(&hir)?;
    let wasm = wasm::compile(&mir)?;
    Ok(wasm)
}

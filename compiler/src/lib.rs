pub mod error;
mod source;
mod mir;
mod semantics;
mod syntax;
mod wasm;

pub fn source<'s>(file_path: String, content: &'s str) -> source::Source<'s> {
    source::Source::new(file_path, content)
}

pub fn parse(source: &source::Source) -> error::Result<syntax::Module> {
    syntax::parse(&source)
}

pub fn compile(source: &source::Source) -> error::Result<Vec<u8>> {
    syntax::parse(&source)
        .and_then(|module| semantics::lower(&module))
        .and_then(|module| mir::lower(&module))
        .and_then(|module| wasm::compile(&module))
}

pub mod error;
pub mod source;
mod mir;
mod syntax;
mod wasm;

pub fn parse(source: &source::Source) -> error::Result<syntax::Module> {
    syntax::parse(&source)
}

pub fn compile(source: &source::Source) -> error::Result<Vec<u8>> {
    syntax::parse(&source)
        .and_then(|module| mir::lower(&module))
        .and_then(|module| wasm::compile(&module))
}

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

pub use source::Source;

pub fn source<'s>(file_path: String, source: &'s str) -> source::Source<'s> {
    source::Source::new(file_path, source)
}

pub fn tokens<'s>(source: &'s source::Source) -> lexer::Tokens<'s> {
    lexer::tokens(source)
}

pub fn parse(source: &source::Source) -> error::Result<syntax::Module> {
    let tokens = tokens(source);
    parser::parse(tokens)
}

pub fn hir(source: &source::Source) -> error::Result<hir::Module> {
    let syntax = parse(source)?;
    hir::from_syntax(&syntax).and_then(type_check::check)
}

pub fn mir(source: &source::Source) -> error::Result<mir::Module> {
    let hir = hir(source)?;
    mir::from_hir(&hir)
}

pub fn compile(source: &source::Source) -> error::Result<Vec<u8>> {
    let mir = mir(source)?;
    wasm::compile(&mir)
}

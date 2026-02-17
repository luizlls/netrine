use compiler::{Compiler, Source};

pub fn compile(source: &str) -> Vec<u8> {
    let source = Source::new("wasm".into(), source.into());
    Compiler::new(&source).compile().unwrap()
}

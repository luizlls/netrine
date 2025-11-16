use compiler::Compiler;

pub fn compile(source: &str) -> Vec<u8> {
    let mut compiler = Compiler::new("wasm".into(), source.into());
    compiler.compile().unwrap()
}

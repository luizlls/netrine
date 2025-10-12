pub fn compile(source: &str) -> Vec<u8> {
    let mut compiler = compiler::Compiler::from_source("wasm".into(), source);
    compiler.compile().unwrap()
}

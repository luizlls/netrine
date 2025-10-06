pub fn compile(source: &str) -> Vec<u8> {
    let source = compiler::source("wasm".to_string(), source);
    let result = compiler::compile(&source);

    result.unwrap()
}

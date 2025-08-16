pub fn compile(source: &str) -> Vec<u8> {
    let source = netrine::source("wasm".to_string(), source);
    let result = netrine::compile(&source);

    result.unwrap()
}

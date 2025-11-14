use compiler::{Compiler, Config, ReportError};

pub fn compile(source: &str) -> Vec<u8> {
    let mut compiler = Compiler::new("wasm".into(), source.into(), Config::new());
    compiler.compile().unwrap_or_report(compiler.source())
}

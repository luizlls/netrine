use std::fs;

use anyhow::Context;

pub fn compile(file_path: String, source: &str) -> anyhow::Result<()> {
    let source = netrine::source(file_path, &source);
    let wasm = netrine::compile(&source)?;

    fs::write("output.wasm", wasm).context("failed to write output file")
}

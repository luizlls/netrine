use std::process::exit;

use anyhow::Context;
use compiler::Compiler;

pub fn compile(file_path: String, source: &str) -> anyhow::Result<Vec<u8>> {
    let mut compiler = Compiler::from_source(file_path, source);

    match compiler.compile() {
        Ok(wasm) => Ok(wasm),
        Err(error) => {
            eprintln!(
                "{}",
                error
                    .report(compiler.source())
                    .context("failed to report error")?
            );
            exit(1);
        }
    }
}

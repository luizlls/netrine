use std::process::exit;

use anyhow::Context;

pub fn compile(file_path: String, source: &str) -> anyhow::Result<Vec<u8>> {
    let source = compiler::source(file_path, source);
    let wasm = compiler::compile(&source);

    match wasm {
        Ok(wasm) => Ok(wasm),
        Err(error) => {
            eprintln!("{}", error.report(&source).context("failed to report error")?);
            exit(1);
        }
    }
}

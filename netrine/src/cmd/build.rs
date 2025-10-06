use std::fs;

use anyhow::Context;

use crate::cmd;

pub fn build(file_path: String, source: &str) -> anyhow::Result<()> {
    let wasm = cmd::compile(file_path, &source)?;
    fs::write("output.wasm", wasm).context("failed to write output file")
}

use std::fmt::{Display, Write};

fn dump(value: impl Display) -> anyhow::Result<String> {
    let mut result = String::new();
    write!(&mut result, "{}", value)?;
    Ok(result)
}

pub fn dump_ast(file_path: String, source: &str) -> anyhow::Result<String> {
    let syntax = compiler::parse(&compiler::source(file_path, source))?;
    dump(syntax)
}

pub fn dump_hir(file_path: String, source: &str) -> anyhow::Result<String> {
    let hir = compiler::hir(&compiler::source(file_path, source))?;
    dump(hir)
}

pub fn dump_mir(file_path: String, source: &str) -> anyhow::Result<String> {
    let mir = compiler::mir(&compiler::source(file_path, source))?;
    dump(mir)
}

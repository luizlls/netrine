pub fn dump_ast(file_path: String, source: &str) -> anyhow::Result<String> {
    let compiler = compiler::Compiler::from_source(file_path, source);
    Ok(compiler.dump_ast()?)
}

pub fn dump_mir(file_path: String, source: &str) -> anyhow::Result<String> {
    let compiler = compiler::Compiler::from_source(file_path, source);
    Ok(compiler.dump_mir()?)
}

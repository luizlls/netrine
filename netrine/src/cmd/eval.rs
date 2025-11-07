use compiler::Compiler;

pub fn eval(file_path: String, source: &str) -> anyhow::Result<String> {
    let compiler = Compiler::from_source(file_path, source);
    Ok(compiler.eval()?)
}

use crate::cmd;

pub fn eval(file_path: String, source: &str) -> anyhow::Result<String> {
    cmd::dump_mir(file_path, source)
}

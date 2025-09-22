use std::fs;
use std::path::PathBuf;

use cli::cmd;

fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() <= 1 {
        cmd::repl()
    } else {
        match args[1].as_str() {
            "e" => exec("<eval>".to_string(), args[2].to_string()),
            "c" => compile("<eval>".to_string(), args[2].to_string()),
            _ => file(args[1].to_string()),
        }
    }
}

fn file(file_path: String) -> anyhow::Result<()> {
    let file_path = PathBuf::from(file_path);
    let content = fs::read_to_string(&file_path).expect("Couldn't open the file");
    let file_path = file_path.display().to_string();

    exec(file_path, content)
}

fn exec(file_path: String, source: String) -> anyhow::Result<()> {
    println!("{}", cmd::eval(file_path, &source)?);
    Ok(())
}

fn compile(file_path: String, source: String) -> anyhow::Result<()> {
    cmd::compile(file_path, &source)
}

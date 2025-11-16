use std::fs;
use std::path::PathBuf;

use compiler::Compiler;

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<String>>();
    if args.is_empty() {
        eprintln!("Usage: netrine [FILE] [-e EXPR]");
        return;
    }

    let (path, source) = if args[0] == "-e" {
        let path = "<eval>".into();
        let source = args[1].clone();
        (path, source)
    } else {
        let path = args[0].clone();
        let source = file(&path);
        (path, source)
    };

    let mut compiler = Compiler::new(path, source);
    match compiler.build() {
        Ok(value) => value,
        Err(error) => {
            let error = error.report(compiler.source()).unwrap();
            eprintln!("{error}");
        }
    }
}

fn file(path: &str) -> String {
    let path = PathBuf::from(path);
    fs::read_to_string(&path).expect("Couldn't open the file")
}

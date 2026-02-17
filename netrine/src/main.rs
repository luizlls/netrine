use std::fs;

use compiler::{Compiler, Source};

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<String>>();
    if args.is_empty() {
        eprintln!("Usage: netrine [FILE]");
        return;
    }

    let source = source(&args[0]);

    let mut compiler = Compiler::new(&source);
    match compiler.parse() {
        Ok(syntax) => {
            println!("{:#?}", syntax);
        }
        Err(error) => {
            let error = error.report(&source).expect("Couldn't report the error");
            eprintln!("{error}");
        }
    }
}

fn source(path: &str) -> Source {
    let content = fs::read_to_string(&path).expect("Couldn't read the file");
    Source::new(path.to_string(), content)
}

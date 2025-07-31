use std::fs;
use std::io::{Write, stdin, stdout};
use std::path::PathBuf;

use compiler::source;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() <= 1 {
        repl();
    } else {
        match args[1].as_str() {
            "e" => {
                eval("repl".to_string(), args[2].to_string());
            }
            _ => {
                file(args[1].to_string());
            }
        }
    }
}

fn file(file: String) {
    let file_path = PathBuf::from(file);
    let content = fs::read_to_string(&file_path).expect("Couldn't open the file");
    let file_path = file_path.display().to_string();
    eval(file_path, content);
}

fn repl() {
    println!("netrine v{}", env!("CARGO_PKG_VERSION"));

    let mut input = String::new();

    while let Ok(line) = read_line() {
        if line.is_empty() {
            eval("repl".to_string(), input.trim_end().to_string());
            input.clear();
        } else {
            input.push_str(&line);
            input.push('\n');
        }
    }
}

fn read_line() -> Result<String, ()> {
    let mut line = String::new();
    print!(">>>> ");

    stdout().flush().unwrap();

    match stdin().read_line(&mut line) {
        Ok(_) => Ok(line.trim_end().to_string()),
        Err(_) => Err(()),
    }
}

fn eval(file_path: String, source: String) {
    let source = source::Source::new(file_path, source);

    match compiler::compile(&source) {
        Ok(wasm) => {
            fs::write("output.wasm", wasm).unwrap();
        }
        Err(error) => {
            let error = error.report(&source).expect("Failed to report error");
            eprintln!("{error}");
        }
    }
}

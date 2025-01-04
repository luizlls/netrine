use std::path::PathBuf;
use std::fs;
use std::io::{stdin, stdout, Write};

use netrine::{Source, SourceId};

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() <= 1 {
        repl();
    } else {
        file(args.get(1).unwrap().to_string());
    }
}

fn file(file: String) {
    let file_path = PathBuf::from(file);
    let content = fs::read_to_string(&file_path).expect("Couldn't open the file");
    let file_path = file_path.display().to_string();
    exec(file_path, content);
}

fn repl() {
    println!("netrine v{}", env!("CARGO_PKG_VERSION"));

    let mut input = String::new();

    while let Ok(line) = read_line() {
        if line.is_empty() {
            exec("repl".to_string(), input.trim_end().to_string());
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

fn exec(file_path: String, source: String) {
    let source = Source::new(SourceId(0), file_path, source);
    let module = netrine::parse(&source);

    if !module.diagnostics.is_empty() {
        let mut buffer = String::new();
        let _ = module.diagnostics.report(&[source], &mut buffer);
        eprintln!("{buffer}");
    }

    for node in module.nodes {
        println!("{node}");
    }
}

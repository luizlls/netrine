#![feature(let_chains)]

// mod arena;
mod error;
mod source;
mod span;
mod syntax;

use std::fs;
use std::io::{stdin, stdout, Write};
use std::path::PathBuf;

use crate::source::Source;
use crate::syntax::parse;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() <= 1 {
        repl();
    } else {
        file(args.get(1).unwrap().to_string());
    }
}

fn file(file: String) {
    let path = PathBuf::from(file);
    let content = fs::read_to_string(&path).expect("Couldn't open the file");
    exec(Source::new(&content, path));
}

fn repl() {
    println!("netrine v{}", env!("CARGO_PKG_VERSION"));

    let mut input = String::new();

    while let Ok(line) = read_line() {
        if line.is_empty() {
            exec(Source::new(input.trim_end(), PathBuf::from("repl")));
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

fn exec(source: Source) {
    println!("{:#?}", parse(&source));
}

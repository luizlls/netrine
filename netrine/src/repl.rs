use std::io::{Write, stdin, stdout};

use compiler::{Compiler, Config};

pub fn repl() {
    println!("netrine v{}", env!("CARGO_PKG_VERSION"));

    let mut input = String::new();

    while let Ok(line) = read_line() {
        if line.is_empty() {
            eval(input.trim_end());
            input.clear();
        } else {
            input.push_str(&line);
            input.push('\n');
        }
    }
}

fn read_line() -> anyhow::Result<String> {
    let mut line = String::new();
    print!(">>>> ");

    stdout().flush().unwrap();

    let _ = stdin().read_line(&mut line)?;
    Ok(line.trim_end().to_string())
}

fn eval(source: &str) {
    let mut compiler = Compiler::new("<repl>".into(), source.into(), Config::new());
    match compiler.eval() {
        Ok(value) => {
            println!("{value}");
        }
        Err(error) => {
            let error = error.report(compiler.source()).unwrap();
            eprintln!("{error}");
        }
    }
}

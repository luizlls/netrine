use std::io::{Write, stdin, stdout};

use crate::cmd;

pub fn repl() -> anyhow::Result<()> {
    println!("netrine v{}", env!("CARGO_PKG_VERSION"));

    let mut input = String::new();

    while let Ok(line) = read_line() {
        if line.is_empty() {
            eval(input.trim_end())?;
            input.clear();
        } else {
            input.push_str(&line);
            input.push('\n');
        }
    }

    Ok(())
}

fn read_line() -> anyhow::Result<String> {
    let mut line = String::new();
    print!(">>>> ");

    stdout().flush().unwrap();

    let _ = stdin().read_line(&mut line)?;
    Ok(line.trim_end().to_string())
}

fn eval(source: &str) -> anyhow::Result<()> {
    println!("{}", cmd::eval("repl".to_string(), source)?);
    Ok(())
}

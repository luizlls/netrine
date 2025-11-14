use std::fs;
use std::path::PathBuf;

use compiler::Config;

fn test_one(pass: &str, path: PathBuf) -> anyhow::Result<()> {
    let file_name = path
        .file_stem()
        .expect("Expected a valid file")
        .to_str()
        .unwrap();

    let source = fs::read_to_string(&path).expect("Could not read file");

    let mut cases = vec![];

    let mut lines = source.lines().peekable();
    while let Some(line) = lines.next() {
        if !line.starts_with("//!") {
            continue;
        }

        let mut input = String::new();
        let test_name = line.split_once("test:").unwrap().1.trim();

        while let Some(line) = lines.next() {
            if line.starts_with("//!") && line.contains("expect") {
                break;
            }
            if !line.is_empty() {
                input.push_str(line);
                input.push('\n');
            }
        }

        let mut output = vec![];

        while let Some(line) = lines.peek() {
            if line.starts_with("//!") && line.contains("test:") || line.starts_with("//") {
                break;
            }
            let line = lines.next().unwrap();
            if !line.is_empty() {
                output.push(line);
            }
        }

        let output = output.join("\n").trim().to_string();

        cases.push((test_name.to_string(), input.trim().to_string(), output));
    }

    for (test_name, input, output) in cases {
        println!("test e2e::{pass}::{file_name}::{test_name}");

        let path = format!("e2e {test_name}");

        let result = {
            match pass {
                // "syntax" => cmd::dump_ast(path, &input),
                // "mir" => cmd::dump_mir(path, &input),
                "eval" => netrine::eval(path, input, Config::new()),
                _ => unreachable!(),
            }

            // match result {
            //     Ok(value) => value,
            //     Err(error) => format!("{error}"),
            // }
        };

        let result = result.trim().to_string();

        if result != output {
            println!("test failed: {pass}::{file_name}::{test_name}");
            println!("left:\n{}", output);
            println!("right:\n{}", result);
            panic!();
        }
    }

    Ok(())
}

fn test(name: &str, path: &str) -> anyhow::Result<()> {
    let paths = fs::read_dir(path).expect("Could not run tests");

    let mut test_paths = vec![];
    for path in paths {
        test_paths.push(path.expect("Expected a valid file").path());
    }

    println!("\nrunning {} {} test files\n", test_paths.len(), name);

    for path in test_paths {
        test_one(name, path)?;
    }

    println!("\nall {name} tests passed!\n");

    Ok(())
}

#[test]
fn e2e() -> anyhow::Result<()> {
    // test("syntax", "./tests/syntax")?;
    // test("hir", "./tests/hir")?;
    // test("mir", "./tests/mir")?;
    test("eval", "./tests/eval")?;

    Ok(())
}

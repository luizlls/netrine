use std::fs;
use std::path::PathBuf;

use netrine::syntax;

fn test_one(base: &str, path: PathBuf) {
    let file_name = path
        .file_stem()
        .expect("Expected a valid file")
        .to_str()
        .unwrap();

    let source = fs::read_to_string(&path).expect("Could not read file");

    let mut cases = Vec::new();
    let mut lines = source.lines().peekable();

    while let Some(line) = lines.next() {
        if !line.starts_with("//") && !line.contains("test:") {
            continue;
        }

        let mut input = String::new();
        let mut output = String::new();
        let test_name = line.split_once("test:").expect("Invalid test comment").1.trim();

        while let Some(line) = lines.next() {
            if line.starts_with("--") {
                break;
            }
            if !line.is_empty() {
                input.push_str(line);
                input.push('\n');
            }
        }

        while let Some(line) = lines.peek() {
            if line.starts_with("//") && line.contains("test:") {
                break;
            }
            let line = lines.next().unwrap();
            if !line.is_empty() {
                output.push_str(line);
                output.push('\n');
            }
        }

        cases.push((test_name.to_string(), input.trim().to_string(), output.trim().to_string()));
    }

    for (test_name, input, output) in cases {
        println!("test {base}::{file_name}::{test_name}");

        let result = match syntax::parse(&input) {
            Ok(nodes) => {
                nodes.into_iter().map(|node| node.dump()).collect::<Vec<_>>().join("\n")
            }
            Err(err) => err.to_string(),
        };

        if result != output {
            println!("Test failed: {base}::{file_name}");
            println!("left:\n{}", output);
            println!("right:\n{}", result);
            panic!();
        }
    }
}

fn test(name: &str, path: &str) {
    let paths = fs::read_dir(path).expect("Could not run tests");

    let mut test_paths = vec![];
    for path in paths {
        test_paths.push(path.expect("Expected a valid file").path());
    }

    println!("\nRunning {} {} tests\n", test_paths.len(), name);

    for path in test_paths {
        test_one(name, path);
    }

    println!("\nAll {name} tests passed!\n");
}

#[test]
fn e2e() {
    test("syntax", "./tests/syntax");
}

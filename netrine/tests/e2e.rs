use std::fs;
use std::path::PathBuf;

use compiler::Compiler;
use wasmtime::{Instance, Module, Store, Val};

fn eval_wasm(source: String) -> String {
    let mut compiler = Compiler::new("test".into(), source);

    let wasm = match compiler.compile() {
        Ok(wasm) => wasm,
        Err(error) => {
            return format!("{error}");
        }
    };

    let mut store = Store::<()>::default();
    let engine = store.engine();
    let module = Module::new(&engine, wasm).unwrap();

    let main = Instance::new(&mut store, &module, &[])
        .unwrap()
        .get_func(&mut store, "main")
        .unwrap();

    let mut results = [Val::F64(0)];
    main.call(&mut store, &[], &mut results).unwrap();

    match results[0] {
        Val::I32(value) => value.to_string(),
        Val::I64(value) => value.to_string(),
        Val::F32(value) => f32::from_bits(value).to_string(),
        Val::F64(value) => f64::from_bits(value).to_string(),
        _ => unreachable!(),
    }
}

fn test_one(pass: &str, path: PathBuf) {
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

        let mut expected = vec![];

        while let Some(line) = lines.peek() {
            if line.starts_with("//!") && line.contains("test:") || line.starts_with("//") {
                break;
            }
            let line = lines.next().unwrap();
            if !line.is_empty() {
                expected.push(line);
            }
        }

        let expected = expected.join("\n").trim().to_string();

        cases.push((test_name.to_string(), input.trim().to_string(), expected));
    }

    for (test_name, input, expected) in cases {
        println!("test e2e::{pass}::{file_name}::{test_name}");

        let result = eval_wasm(input).trim().to_string();

        if result != expected {
            println!("test failed: {pass}::{file_name}::{test_name}");
            println!("left:\n{}", expected);
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

    println!("\nrunning {} {} test files\n", test_paths.len(), name);

    for path in test_paths {
        test_one(name, path);
    }

    println!("\nall {name} tests passed!\n");
}

#[test]
fn e2e() {
    test("eval", "./tests/eval");
}

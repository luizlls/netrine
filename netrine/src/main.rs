use std::fs;
use std::path::PathBuf;

use compiler::Config;
use getopts::{HasArg, Occur, Options};

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<String>>();
    run(args)
}

fn run(args: Vec<String>) {
    if args.is_empty() {
        return netrine::repl();
    }

    match args[0].as_str() {
        "eval" => eval(args),
        "build" => build(args),
        _ => help(),
    }
}

fn eval(args: Vec<String>) {
    let (path, source, config) = parse(&args[1..]);
    let value = netrine::eval(path, source, config);
    println!("{value}");
}

fn build(args: Vec<String>) {
    let (path, source, config) = parse(&args[1..]);
    netrine::build(path, source, config);
}

fn options() -> Options {
    let mut options = Options::new();
    options.opt("o", "output", "Output file path", "FILE", HasArg::Yes, Occur::Optional);
    options.opt("e", "expr", "Expression to be evaluated", "EXPR", HasArg::Yes, Occur::Optional);
    options.optflag("", "ast", "Dump the AST");
    options.optflag("", "hir", "Dump the HIR");
    options.optflag("", "mir", "Dump the MIR");
    options.optflag("h", "help", "Print this help menu");

    options
}

fn parse(args: &[String]) -> (String, String, Config) {
    let matches = match options().parse(args) {
        Ok(matches) => matches,
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1);
        }
    };

    if matches.opt_present("help") {
        help();
        std::process::exit(0);
    }

    let mut config = Config::new();

    if let Some(output) = matches.opt_str("output") {
        config.output = output.into();
    }
    config.dump_ast = matches.opt_present("ast");
    config.dump_hir = matches.opt_present("hir");
    config.dump_mir = matches.opt_present("mir");

    let (path, source) = if let Some(path) = matches.free.first() {
        let source = file(path);
        (path.clone(), source)
    } else if let Some(source) = matches.opt_str("expr") {
        ("<eval>".into(), source)
    } else {
        eprintln!("Missing file path or --expr argument");
        std::process::exit(1);
    };

    (path, source, config)
}

fn file(path: &str) -> String {
    let path = PathBuf::from(path);
    fs::read_to_string(&path).expect("Couldn't open the file")
}

fn help() {
    let brief = "Usage: netrine COMMAND [FILE] [OPTIONS]
Commands:

build    Compiles and emits a WASM binary
eval     Evaluate an expression";

    print!("{}", options().usage(&brief))
}

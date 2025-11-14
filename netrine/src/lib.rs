mod repl;

pub use repl::repl;

use compiler::{Compiler, Config, ReportError};

pub fn compile(path: String, source: String, config: Config) -> Vec<u8> {
    let mut compiler = Compiler::new(path, source, config);
    compiler.compile().unwrap_or_report(compiler.source())
}

pub fn build(path: String, source: String, config: Config) {
    let mut compiler = Compiler::new(path, source, config);
    compiler.build().unwrap_or_report(compiler.source())
}

pub fn eval(path: String, source: String, config: Config) -> String {
    let mut compiler = Compiler::new(path, source, config);
    compiler.eval().unwrap_or_report(compiler.source())
}

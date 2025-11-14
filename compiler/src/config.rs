const DEFAULT_OUTPUT: &'static str = "output.wasm";

#[derive(Debug)]
pub struct Config {
    pub output: String,
    pub dump_ast: bool,
    pub dump_hir: bool,
    pub dump_mir: bool,
}

impl Config {
    pub fn new() -> Config {
        Config {
            output: DEFAULT_OUTPUT.into(),
            dump_ast: false,
            dump_hir: false,
            dump_mir: false,
        }
    }
}

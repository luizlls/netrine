use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct Source {
    pub path: PathBuf,
    pub content: String,
}

impl Source {
    pub fn new(content: &str, path: PathBuf) -> Source {
        Source {
            path,
            content: content.into(),
        }
    }

    pub fn source(content: &str) -> Source {
        Source::new(content, PathBuf::from("none"))
    }
}

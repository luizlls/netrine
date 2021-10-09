#![feature(box_syntax)]
#![feature(box_patterns)]
#![macro_use]

pub mod syntax;
pub mod error;

use std::{ops::Range, path::PathBuf};

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

#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub struct Span {
    line: u32,
    start: u32,
    end: u32,
}

impl Default for Span {
    fn default() -> Span {
        Span {
            line: 0, start: 0, end: 0
        }
    }
}

impl Span {
    pub const fn new(line: u32, start: u32, end: u32) -> Span {
        Span {
            line, start, end
        }
    }

    pub const fn from(left: Span, right: Span) -> Span {
        Span {
            end: right.end, ..left
        }
    }

    pub const fn range(self) -> Range<usize> {
        (self.start as usize) .. (self.end as usize)
    }
}

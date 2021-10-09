#![feature(box_syntax)]
#![feature(box_patterns)]
#![macro_use]

pub mod syntax;
pub mod error;

use std::{fmt, ops::Range, path::PathBuf};

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

#[derive(Clone, Copy, Hash, PartialEq)]
pub struct Span {
    start: usize, end: usize
}

impl Default for Span {
    fn default() -> Span {
        Span {
            start: 0, end: 0
        }
    }
}

impl Span {
    pub const fn new(start: usize, end: usize) -> Span {
        Span {
            start, end
        }
    }

    pub const fn from(left: Span, right: Span) -> Span {
        Span {
            start: left.start, end: right.end
        }
    }

    pub const fn range(self) -> Range<usize> {
        (self.start as usize) .. (self.end as usize)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.start.fmt(fmt)?;
        write!(fmt, "..")?;
        self.end.fmt(fmt)?;
        Ok(())
    }
}

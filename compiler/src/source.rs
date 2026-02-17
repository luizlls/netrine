use std::fmt::Display;
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Source {
    pub path: String,
    pub content: String,
}

impl Source {
    pub fn new(path: String, content: String) -> Source {
        Source { path, content }
    }

    pub fn slice(&self, span: Span) -> &str {
        &self.content[span.start as usize..span.end as usize]
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub const ZERO: Span = Span::new(0, 0);

    pub const fn new(start: u32, end: u32) -> Span {
        Span { start, end }
    }

    pub const fn from(from: Span, to: Span) -> Span {
        Span {
            start: from.start,
            end: to.end,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Range<usize> {
        span.range()
    }
}

impl Span {
    pub fn range(self) -> Range<usize> {
        (self.start as usize)..(self.end as usize)
    }
}

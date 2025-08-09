use std::fmt::Display;
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Source<'s> {
    pub(crate) file_path: String,
    pub(crate) content: &'s str,
}

impl<'s> Source<'s> {
    pub fn new(file_path: String, content: &str) -> Source<'_> {
        Source { file_path, content }
    }

    pub fn slice(&self, span: Span) -> &str {
        &self.content[span.range()]
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

pub trait ToSpan {
    fn span(&self) -> Span;
}

impl Span {
    pub fn new(start: u32, end: u32) -> Span {
        Span { start, end }
    }

    pub fn from(from: &impl ToSpan, to: &impl ToSpan) -> Span {
        Span {
            start: from.span().start,
            end: to.span().end,
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

impl ToSpan for Span {
    fn span(&self) -> Span {
        *self
    }
}

use std::fmt::Display;
use std::ops::Range;
use std::usize;

#[derive(Debug, Clone)]
pub struct Source {
    pub source_id: SourceId,
    pub file_path: String,
    pub content: String,
}

impl Source {
    pub fn new(source_id: SourceId, file_path: String, content: String) -> Source {
        Source {
            source_id,
            file_path,
            content,
        }
    }

    pub fn slice(&self, span: Span) -> &str {
        &self.content[span.range()]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(pub usize);

impl SourceId {
    pub fn index(self) -> usize {
        self.0
    }

    pub fn default() -> SourceId {
        SourceId(usize::MAX)
    }
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Span {
        Span { start, end }
    }

    pub fn of(from: Span, to: Span) -> Span {
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

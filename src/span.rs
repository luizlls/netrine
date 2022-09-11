use std::ops::Range;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Default)]
pub struct Span {
    pub line: u32,
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn from(start: Span, end: Span) -> Span {
        Span {
            line: start.line,
            start: start.start,
            end: end.end
        }
    }

    pub fn range(self) -> Range<usize> {
        (self.start as usize)..(self.end as usize)
    }
}

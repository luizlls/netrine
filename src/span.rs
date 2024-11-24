use std::fmt::Display;
use std::ops::Range;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn to(self, end: Span) -> Span {
        Span {
            lo: self.lo,
            hi: end.hi,
        }
    }

    pub fn from(start: Span, end: Span) -> Span {
        start.to(end)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.lo, self.hi)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Range<usize> {
        span.range()
    }
}

impl Span {
    pub fn range(self) -> Range<usize> {
        (self.lo as usize)..(self.hi as usize)
    }
}

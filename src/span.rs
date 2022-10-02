use std::fmt::Display;
use std::ops::Range;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct Span(pub u32, pub u32);

impl Span {
    pub fn of(
        left: &impl WithSpan,
        right: &impl WithSpan,
    ) -> Span {
        Span(left.span().0, right.span().1)
    }

    pub fn start(&self) -> u32 {
        self.0
    }

    pub fn end(&self) -> u32 {
        self.1
    }
}

impl WithSpan for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} .. {}", self.0, self.1)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Range<usize> {
        span.range()
    }
}

impl Span {
    pub fn range(&self) -> Range<usize> {
        (self.0 as usize) .. (self.1 as usize)
    }
}

pub trait WithSpan {
    fn span(&self) -> Span;
}

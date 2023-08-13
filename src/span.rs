use std::fmt::Display;
use std::ops::Range;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct Span(pub u32, pub u32);

impl Span {
    pub fn from(
        lo: &impl IntoSpan,
        hi: &impl IntoSpan,
    ) -> Span {
        Span(lo.span().lo(), hi.span().hi())
    }

    pub fn lo(self) -> u32 {
        self.0
    }

    pub fn hi(self) -> u32 {
        self.1
    }
}

pub trait IntoSpan {
    fn span(&self) -> Span;
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.0, self.1)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Range<usize> {
        span.range()
    }
}

impl Span {
    pub fn range(self) -> Range<usize> {
        (self.0 as usize) .. (self.1 as usize)
    }
}

impl IntoSpan for Span {
    fn span(&self) -> Span {
        *self
    }
}

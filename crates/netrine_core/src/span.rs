use std::ops::Range;

#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub struct Span {
    pub line: u32,
    pub start: u32,
    pub end: u32,
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

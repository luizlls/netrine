use std::error::Error;
use std::fmt;
use std::fmt::{Formatter, Write, Display};

use crate::{Source, Span};

pub type Result<T> = ::std::result::Result<T, NetrineError>;

#[derive(Debug, Clone)]
pub struct NetrineError {
    text: String,
    span: Option<Span>
}

impl NetrineError {

    pub fn error(span: Span, text: String) -> NetrineError {
        NetrineError { text, span: Some(span) }
    }

    pub fn basic(text: String) -> NetrineError {
        NetrineError { text, span: None }
    }

    pub fn with_span(self, span: Span) -> NetrineError {
        NetrineError { span: Some(span), ..self }
    }
}

impl Error for NetrineError {}

impl Display for NetrineError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "error")?;

        if let Some(span) = self.span {
            write!(f, " [{}..{}]", span.start, span.end)?;
        }

        write!(f, ": {}", self.text)
    }
}

impl NetrineError {
    pub fn report(&self, source: &Source, buf: &mut String) -> fmt::Result {
        if self.span.is_none() {
            return writeln!(buf, "error: {}", self.text)
        }

        let path = source.path.as_path().display().to_string();
        let span = self.span.unwrap();

        let (left, right) = source.content.split_at(span.start as usize);

        let mut left = left.rsplit('\n');
        let left_main = left.next().unwrap();
        let mut right = right.split('\n');
        let right_main = right.next().unwrap();

        let padding_length = (span.line  + 1).to_string().len() as usize;
        let number_padding = " ".repeat(padding_length);
        let pointer_padding = " ".repeat(left_main.len());
        let pointer_arrows = "^".repeat((span.end - span.start).max(1) as usize);

        let format_line = |n: u32| {
            format!("{:>length$}", n, length=padding_length)
        };

        writeln!(buf, "\nerror: {}", self.text)?;

        writeln!(buf, "{}--> {} at line {}", number_padding, path, span.line)?;
        writeln!(buf, "{} |", number_padding)?;
        
        if let Some(prev_line) = left.next() {
            if !prev_line.trim().is_empty() {
                writeln!(buf, "{} | {}", format_line(span.line - 1), prev_line)?;
            }
        }

        writeln!(buf, "{} | {}{}", format_line(span.line), left_main, right_main)?;
        writeln!(buf, "{} | {}{}", number_padding, pointer_padding, pointer_arrows)?;

        if let Some(next_line) = right.next() {
            if !next_line.trim().is_empty() {
                writeln!(buf, "{} | {}", format_line(span.line + 1), next_line)?;
            }
        }

        writeln!(buf, "{} |", number_padding)?;

        writeln!(buf, "\n")?;
        writeln!(buf, "error: could not compile {}", path)
    }
}

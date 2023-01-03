use std::fmt;
use std::fmt::{Display, Formatter, Write};

use crate::source::Source;
use crate::span::Span;

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Error {
    text: String,
    span: Option<Span>,
}

impl Error {
    pub fn new<S: Into<String>>(msg: S, span: Span) -> Error {
        Error {
            text: msg.into(),
            span: Some(span),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "error")?;

        if let Some(Span(start, end)) = self.span {
            write!(f, " [{}..{}]", start, end)?;
        }

        write!(f, ": {}", self.text)
    }
}

impl Error {
    pub fn report(&self, source: &Source, buf: &mut String) -> fmt::Result {
        if self.span.is_none() {
            return writeln!(buf, "error: {}", self.text);
        }

        let path = source.path.as_path().display().to_string();

        let span = self.span.unwrap();
        let Span(start, end) = span;
        let line = self.find_line(source, start as usize);

        let (left, right) = source.content.split_at(start as usize);

        let mut left = left.rsplit('\n');
        let left_main = left.next().unwrap();
        let mut right = right.split('\n');
        let right_main = right.next().unwrap();

        let padding_length = (line + 1).to_string().len() as usize;
        let number_padding = " ".repeat(padding_length);
        let pointer_padding = " ".repeat(left_main.len());
        let pointer_arrows = "^".repeat((end - start).max(1) as usize);

        let format_line = |n: usize| format!("{:>length$}", n, length = padding_length);

        writeln!(buf, "\nerror: {}", self.text)?;

        writeln!(buf, "{}--> {} at line {}", number_padding, path, line)?;
        writeln!(buf, "{} |", number_padding)?;

        if let Some(prev_line) = left.next() {
            if !prev_line.trim().is_empty() {
                writeln!(buf, "{} | {}", format_line(line - 1), prev_line)?;
            }
        }

        writeln!(buf, "{} | {}{}", format_line(line), left_main, right_main)?;
        writeln!(buf, "{} | {}{}", number_padding, pointer_padding, pointer_arrows)?;

        if let Some(next_line) = right.next() {
            if !next_line.trim().is_empty() {
                writeln!(buf, "{} | {}", format_line(line + 1), next_line)?;
            }
        }

        writeln!(buf, "{} |", number_padding)?;

        writeln!(buf, "\n")?;
        writeln!(buf, "error: could not compile {}", path)
    }

    fn find_line(&self, source: &Source, position: usize) -> usize {
        let mut line = 1;

        for (idx, ch) in source.content.chars().enumerate() {
            if idx == position {
                return line;
            } else if ch == '\n' {
                line += 1;
            }
        }

        line
    }
}

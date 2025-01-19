use std::fmt;
use std::fmt::{Display, Formatter, Write};

use crate::source::{Source, Span};

pub type Result<T, E = Error> = ::std::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    error: String,
    span: Option<Span>,
}

impl Error {
    pub fn new(error: String, span: Span) -> Error {
        Error {
            error,
            span: Some(span),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "error: {}", self.error)
    }
}

impl Error {
    pub fn report(&self, source: &Source, buf: &mut String) -> fmt::Result {

        let Some(Span { start, end }) = self.span else {
            return writeln!(buf, "error: {}\n", self.error);
        };

        let Source {
            content,
            file_path,
            ..
        } = source;

        let (line, column) = Self::find_line(&content, start as usize);
        let (left, right) = content.split_at(start as usize);

        let mut left = left.rsplit('\n');
        let left_main = left.next().unwrap();
        let mut right = right.split('\n');
        let right_main = right.next().unwrap();

        let padding_length = (line + 1).to_string().len();
        let number_padding = " ".repeat(padding_length);
        let pointer_padding = " ".repeat(left_main.len());
        let pointer_arrows = "^".repeat((end - start).max(1) as usize);

        let format_line = |n: usize| format!("{n:>padding_length$}");

        writeln!(buf, "\nerror: {}\n", self.error)?;

        writeln!(buf, "{number_padding}--> {file_path} at {line}:{column}")?;
        writeln!(buf, "{number_padding} |")?;

        if let Some(prev_line) = left.next() {
            if !prev_line.trim().is_empty() {
                writeln!(buf, "{} | {prev_line}", format_line(line - 1))?;
            }
        }

        writeln!(buf, "{} | {left_main}{right_main}", format_line(line))?;
        writeln!(buf, "{number_padding} | {pointer_padding}{pointer_arrows}")?;

        if let Some(next_line) = right.next() {
            if !next_line.trim().is_empty() {
                writeln!(buf, "{} | {}", format_line(line + 1), next_line)?;
            }
        }

        writeln!(buf, "{number_padding} |")?;

        writeln!(buf, "\n")?;
        writeln!(buf, "error: could not compile {file_path}")
    }

    fn find_line(source: &str, position: usize) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;

        for (idx, ch) in source.chars().enumerate() {
            if idx == position {
                break;
            } else if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }

        (line, col)
    }
}

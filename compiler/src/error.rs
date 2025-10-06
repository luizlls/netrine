use std::fmt::{self, Display, Write};

use crate::source::{Source, Span};

pub type Result<T, E = Error> = ::std::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    error: String,
    span: Option<Span>,
}

impl Error {
    pub fn error(span: Span, error: String) -> Error {
        Error {
            error,
            span: Some(span),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error: {}", self.error)
    }
}

impl std::error::Error for Error {}

impl Error {
    pub fn report(&self, source: &Source) -> Result<String, std::fmt::Error> {
        let mut buf = String::new();

        let Some(Span { start, end }) = self.span else {
            writeln!(buf, "error: {}\n", self.error)?;
            return Ok(buf);
        };

        let Source {
            content,
            file_path: path,
            ..
        } = source;

        let (line, column) = Self::find_line(content, start as usize);
        let (before, after) = content.split_at(start as usize);

        let before = before.rsplit('\n').next().unwrap();
        let after = after.split('\n').next().unwrap();

        let number_length = line.to_string().len();
        let number_padding = " ".repeat(number_length);
        let pointer_padding = " ".repeat(before.len());
        let pointer_arrows = "^".repeat((end - start).max(1) as usize);

        writeln!(buf, "\nerror: {}\n", self.error)?;

        writeln!(buf, "{number_padding}--> {path} at {line}:{column}")?;
        writeln!(buf, "{number_padding} |")?;

        writeln!(buf, "{line:>number_length$} | {before}{after}")?;
        writeln!(buf, "{number_padding} | {pointer_padding}{pointer_arrows}")?;

        writeln!(buf, "{number_padding} |")?;

        writeln!(buf, "\n")?;
        writeln!(buf, "error: could not compile {path}")?;

        Ok(buf)
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

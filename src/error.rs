use std::error::Error;
use std::fmt;
use std::fmt::Display;

use crate::Span;

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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error")?;

        if let Some(span) = self.span {
            write!(f, " [{}]", span.line)?;
        }

        write!(f, ": {}", self.text)
    }
}

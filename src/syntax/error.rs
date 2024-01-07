use std::fmt;

use crate::error::{self, Error};
use crate::span::Span;

use super::token::TokenKind;

pub type Result<T> = error::Result<T, SyntaxError>;

pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxErrorKind {
    UnexpectedToken(TokenKind, [TokenKind; 4]),
    ExpectedItem,
    ExpectedExpr,
    ExpectedPatt,
    UnexpectedCharacter,
    UnterminatedString,
}

impl SyntaxError {
    pub fn new(kind: SyntaxErrorKind, span: Span) -> SyntaxError {
        SyntaxError {
            kind,
            span,
        }
    }

    pub fn unexpected(found: TokenKind, expected: &[TokenKind], span: Span) -> SyntaxError {
        let mut arr = [TokenKind::Noop; 4];
        for (idx, kind) in expected.iter().take(4).enumerate() {
            arr[idx] = *kind;
        }
        SyntaxError::new(SyntaxErrorKind::UnexpectedToken(found, arr), span)
    }
}

impl fmt::Display for SyntaxErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let formatted;
        let description = match self {
            SyntaxErrorKind::ExpectedItem => "expected a module item",
            SyntaxErrorKind::ExpectedExpr => "expected an expression",
            SyntaxErrorKind::ExpectedPatt => "expected a pattern",
            SyntaxErrorKind::UnexpectedCharacter => "unexpected character",
            SyntaxErrorKind::UnterminatedString  => "unterminated string",
            SyntaxErrorKind::UnexpectedToken(found, expected) => {
                let expected = expected
                    .iter()
                    .take_while(|&kind| kind != &TokenKind::Noop)
                    .map(|it| {
                        format!("`{it}`")
                    })
                    .collect::<Vec<_>>()
                    .join(" or ");
                formatted = format!("expected {expected}, but found `{found}`");
                &formatted
            },
        };
        write!(f, "{description}")
    }
}

impl From<SyntaxError> for Error {
    fn from(value: SyntaxError) -> Self {
        Error::new(value.kind.to_string(), value.span)
    }
}

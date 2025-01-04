use std::{collections::HashMap, fmt::{self, Write}};

use crate::source::{Source, SourceId, Span};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    span: Option<Span>,
    source_id: SourceId,
}

#[derive(Debug, Clone)]
pub enum DiagnosticKind {
    Error(String),
    Warning(String),
}

impl Diagnostic {
    fn report(&self, source: &Source, (line, column): (usize, usize), buf: &mut String) -> fmt::Result {
        let (kind, message) = match &self.kind {
            DiagnosticKind::Error(message) => ("error", message),
            DiagnosticKind::Warning(message) => ("warning", message),
        };

        if self.span.is_none() {
            return writeln!(buf, "{kind}: {message}\n");
        }

        let Span {
            start,
            end,
        } = self.span.unwrap();

        let Source {
            content,
            file_path,
            ..
        } = source;

        let (before, after) = content.split_at(start as usize);

        let mut before = before.rsplit('\n');
        let before_main = before.next().unwrap();
        let mut after = after.split('\n');
        let after_main = after.next().unwrap();

        let padding_length = (line + 1).to_string().len();
        let number_padding = " ".repeat(padding_length);
        let pointer_padding = " ".repeat(before_main.len());
        let pointer_arrows = "^".repeat((end - start).max(1) as usize);

        let format_line = |n: usize| format!("{n:>padding_length$}");

        writeln!(buf, "\n{kind}: {message}\n")?;

        writeln!(buf, "{number_padding}--> {file_path} at {line}:{column}")?;
        writeln!(buf, "{number_padding} |")?;

        if let Some(prev_line) = before.next() {
            if !prev_line.trim().is_empty() {
                writeln!(buf, "{} | {prev_line}", format_line(line - 1))?;
            }
        }

        writeln!(buf, "{} | {before_main}{after_main}", format_line(line))?;
        writeln!(buf, "{number_padding} | {pointer_padding}{pointer_arrows}")?;

        if let Some(next_line) = after.next() {
            if !next_line.trim().is_empty() {
                writeln!(buf, "{} | {}", format_line(line + 1), next_line)?;
            }
        }

        writeln!(buf, "{number_padding} |")
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostics {
    source_id: SourceId,
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new(source_id: SourceId) -> Diagnostics {
        Diagnostics {
            source_id,
            diagnostics: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub fn error(&mut self, error: String, span: Span) {
        self.diagnostics.push(
            Diagnostic {
                kind: DiagnosticKind::Error(error),
                span: Some(span),
                source_id: self.source_id,
            }
        );
    }

    pub fn warning(&mut self, warning: String, span: Span) {
        self.diagnostics.push(
            Diagnostic {
                kind: DiagnosticKind::Warning(warning),
                span: Some(span),
                source_id: self.source_id,
            }
        );
    }

    pub fn basic(&mut self, error: String) {
        self.diagnostics.push(
            Diagnostic {
                kind: DiagnosticKind::Error(error),
                span: None,
                source_id: self.source_id,
            }
        );
    }

    pub fn report(&self, sources: &[Source], buf: &mut String) -> fmt::Result {
        let diagnostics = self.diagnostics.iter().fold(
            HashMap::new(),
            |mut diagnostics, diagnostic| {
                diagnostics.entry(diagnostic.source_id).or_insert_with(Vec::new).push(diagnostic);
                diagnostics
            }
        );

        for (source, mut diagnostics) in diagnostics {
            let source = &sources[source.index()];

            diagnostics.sort_by_key(|&diagnostic| diagnostic.span.map_or(0, |span| span.start));

            let mut line = 1;
            let mut col = 1;
            let mut chars = source.content.chars().enumerate();

            for diagnostic in diagnostics {
                let position = if let Some(span) = diagnostic.span {
                    let target = span.start as usize;
                    loop {
                        let (idx, chr) = chars.next().expect("Diagnostic span out of bounds");
                        if idx == target {
                            break (line, col);
                        }
                        if chr == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                    }
                } else {
                    (0, 0)
                };

                diagnostic.report(source, position, buf)?;
            }
        }

        Ok(())
    }
}

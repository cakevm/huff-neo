use crate::file::file_source::FileSource;
use serde::{Deserialize, Serialize};
use std::ops::{Add, Range};
use std::sync::Arc;

/// A Span is a section of a source file.
#[derive(Default, Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Span {
    /// The start of the span.
    pub start: usize,
    /// The end of the span.
    pub end: usize,
    /// The Associated File
    pub file: Option<Arc<FileSource>>,
}

impl Span {
    /// An EOF spans [0, 0].
    pub const EOF: Span = Span { start: 0, end: 0, file: None };

    /// Public associated function to instantiate a new span.
    pub fn new(Range { start, end }: Range<usize>, file: Option<Arc<FileSource>>) -> Self {
        Self { start, end, file }
    }

    /// Converts a span to a range.
    pub fn range(&self) -> Option<Range<usize>> {
        (*self != Self::EOF).then_some(self.start..self.end)
    }

    /// Produces a file identifier string for errors
    pub fn identifier(&self) -> String {
        self.file.as_ref().map(|f| format!("\n-> {}", f.path)).unwrap_or_default()
    }

    /// Produces a source segment string
    pub fn source_seg(&self) -> String {
        self.file
            .as_ref()
            .map(|f| {
                f.source
                    .as_ref()
                    .map(|s| {
                        if self.start >= s.len() {
                            // This should never happen, but currently does when the mapping from the flattened source is incorrect.
                            return format!("\nInternal compiler error: Start index out of range start={} len={}.", self.start, s.len());
                        }
                        let line_num = &s[0..self.start].as_bytes().iter().filter(|&&c| c == b'\n').count() + 1;
                        let line_start = &s[0..self.start].rfind('\n').unwrap_or(0);
                        let line_end = self.end + s[self.end..s.len()].find('\n').unwrap_or(s.len() - self.end).to_owned();
                        let padding = (0..line_num.to_string().len()).map(|_| " ").collect::<String>();
                        format!(
                            "\n     {}|\n  > {} | {}\n     {}|",
                            padding,
                            line_num,
                            &s[line_start.to_owned()..line_end].replace('\n', ""),
                            padding
                        )
                    })
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.range().unwrap()
    }
}

impl From<Range<usize>> for Span {
    fn from(Range { start, end }: Range<usize>) -> Self {
        Self { start, end, file: None }
    }
}

impl Add for Span {
    type Output = Span;

    fn add(self, rhs: Span) -> Self::Output {
        Span::new(self.start..rhs.end, None)
    }
}

/// Spanned trait requires a type to have a span.
pub trait Spanned {
    /// Returns a Span.
    fn span(&self) -> Span;
}

/// WithSpan associates a value to a Span.
pub struct WithSpan<T> {
    /// The value
    pub value: T,
    /// The associated Span
    pub span: Span,
}

impl<T> WithSpan<T> {
    /// Public associated function to instatiate a new WithSpan.
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}

impl<T> Spanned for WithSpan<T> {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

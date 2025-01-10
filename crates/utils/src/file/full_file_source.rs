use crate::file::file_source::FileSource;
use crate::prelude::Span;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

#[allow(clippy::to_string_in_format_args)]
/// An aliased output location to derive from the cli arguments.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
pub struct OutputLocation(pub String);

/// Full File Source
#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct FullFileSource<'a> {
    /// Flattened file source
    pub source: &'a str,
    /// The top level file source
    pub file: Option<Arc<FileSource>>,
    /// Files and their associated spans in the flattend file source
    pub spans: Vec<(Arc<FileSource>, Span)>,
}

impl FullFileSource<'_> {
    /// Get the relative span by position. Because the file source is flattened, we need to find the correct file source.
    pub fn relative_span_by_pos(&self, start: usize, end: usize) -> Span {
        // If spans are empty we have a single file source e.g. for tests
        if self.spans.is_empty() {
            return Span { start, end, file: self.file.clone() };
        }
        let span_opt = self
            .spans
            .iter()
            .filter(|s| s.1.start <= start && s.1.end >= end)
            .map(|s| Span { start: start - s.1.start, end: end - s.1.start, file: Some(s.0.clone()) })
            .collect::<Vec<Span>>()
            .into_iter()
            .next();
        if let Some(span) = span_opt {
            span
        } else {
            Span { start, end, file: self.file.clone() }
        }
    }
}

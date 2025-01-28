use crate::prelude::Span;
use itertools::Itertools;
use std::collections::BTreeMap;
use std::ops::Index;

/// An AST-level Span
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AstSpan(pub Vec<Span>);

impl AstSpan {
    /// Coalesce Multiple Spans Into an error string
    pub fn error(&self, hint: Option<&String>) -> String {
        let file_to_source_map = self.0.iter().fold(BTreeMap::<String, Vec<&Span>>::new(), |mut m, s| {
            let file_name = s.file.as_ref().map(|f2| f2.path.clone()).unwrap_or_default();
            let mut new_vec: Vec<&Span> = m.get(&file_name).cloned().unwrap_or_default();
            new_vec.push(s);
            m.insert(file_name, new_vec);
            m
        });
        let source_str = file_to_source_map.iter().filter(|fs| !fs.0.is_empty()).fold("".to_string(), |s, fs| {
            let start = fs.1.iter().map(|fs2| fs2.start).min().unwrap_or(0);
            let end = fs.1.iter().map(|fs2| fs2.end).max().unwrap_or(0);
            let newline_s = if s.is_empty() { "".to_string() } else { format!("{s}\n") };
            if start.eq(&0) && end.eq(&0) {
                format!("{newline_s}-> {}\n   > 0|", fs.0)
            } else {
                format!(
                    "{}-> {}:{}",
                    newline_s,
                    fs.0,
                    fs.1.iter()
                        .map(|sp| sp.source_seg())
                        .filter(|ss| !ss.is_empty())
                        .collect::<Vec<String>>()
                        .into_iter()
                        .unique()
                        .fold("".to_string(), |acc, ss| { format!("{acc}{ss}") })
                )
            }
        });

        // Add in optional hint message
        format!("{}{source_str}", hint.map(|msg| format!("{msg}\n")).unwrap_or_default())
    }

    /// Print just the file for missing
    pub fn file(&self) -> String {
        self.0.iter().fold("".to_string(), |acc, span| match &span.file {
            Some(fs) => format!("-> {}\n{acc}", fs.path),
            None => Default::default(),
        })
    }

    /// Retrieve the underlying vector of spans
    pub fn inner_ref(&self) -> &Vec<Span> {
        &self.0
    }
}

/// Allows AstSpan to be indexed into
impl Index<usize> for AstSpan {
    type Output = Span;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

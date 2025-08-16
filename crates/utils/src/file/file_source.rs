use crate::prelude::Span;
use crate::time;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fmt;
use std::path::PathBuf;
use std::sync::Arc;

/// File Encapsulation
#[derive(Default, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct FileSource {
    /// File Path
    pub path: String,
    /// File Source
    pub source: Option<String>,
    /// Last File Access Time
    pub access: Option<time::Time>,
    /// An Ordered List of File Dependencies
    pub dependencies: Vec<Arc<FileSource>>,
}

// Reduce the size of the debug output by not printing the source code and dependencies
impl fmt::Debug for FileSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FileSource")
            .field("path", &self.path)
            .field("source", &"...")
            .field("access", &self.access)
            .field("dependencies", &"...")
            .finish()
    }
}

impl FileSource {
    /// Visit all dependencies and return a list of none duplicate [FileSource] in order of occurrence.
    fn discover_all_file_sources(node_file_source: Arc<FileSource>, visited_nodes: &mut HashSet<String>) -> Vec<Arc<FileSource>> {
        if visited_nodes.contains(&node_file_source.path) {
            return vec![];
        }
        visited_nodes.insert(node_file_source.path.clone());

        let mut result = vec![];
        for child_file_source in node_file_source.dependencies.iter() {
            let mut child_result = FileSource::discover_all_file_sources(child_file_source.clone(), visited_nodes);
            result.append(&mut child_result);
        }

        result.push(node_file_source);

        result
    }

    /// Generates a fully flattened source code for the given `FileSource` and all its dependencies
    ///
    /// ### Examples
    ///
    /// Let's say you have a file, `a.txt` with two dependencies, `b.txt` and `c.txt`,
    /// `fully_flatten()` will generate a source code string with the contents of `b.txt` and
    /// `c.txt` appended to the end of the contents of `a.txt`.
    ///
    /// Note: This preserves #include statements in the output to maintain correct span positions
    pub fn fully_flatten(node_file_source: Arc<FileSource>) -> (String, Vec<(Arc<FileSource>, Span)>) {
        let all_file_sources = FileSource::discover_all_file_sources(node_file_source, &mut HashSet::new());

        let mut full_source = "".to_string();
        let mut relative_positions = vec![];
        let mut shift_pos = 0;

        // Append all child elements
        for (i, child_file_source) in all_file_sources.into_iter().enumerate() {
            let Some(source) = child_file_source.source.clone() else {
                continue;
            };

            // Add a newline before each file after the first to ensure proper separation
            if i > 0 && !full_source.ends_with('\n') {
                full_source.push('\n');
                shift_pos += 1;
            }

            let span = Span::new(shift_pos..(shift_pos + source.len() - 1), Some(child_file_source.clone()));
            relative_positions.push((child_file_source, span));
            shift_pos += source.len();
            full_source = format!("{full_source}{source}");
        }

        (full_source, relative_positions)
    }

    /// Derives a File Path's directory
    pub fn derive_dir(path: &str) -> Option<String> {
        let path = PathBuf::from(path);
        match path.parent() {
            Some(p) => p.to_str().map(String::from),
            None => None,
        }
    }

    /// Localizes a file path, if path is relative
    pub fn localize_file(parent: &str, child: &str) -> Option<String> {
        let mut prefixed_parent;
        if !parent.starts_with('.') {
            prefixed_parent = "./".to_owned();
            prefixed_parent.push_str(parent);
        } else {
            prefixed_parent = parent.to_owned();
        }
        let mut prefix = match FileSource::derive_dir(prefixed_parent.as_str()) {
            Some(p) => {
                if p.is_empty() {
                    String::from(".")
                } else {
                    p
                }
            }
            None => String::from("."),
        };
        if child.starts_with("../") {
            let mut res_str = child.to_string();
            while res_str.starts_with("../") {
                let path = PathBuf::from(prefix.clone());
                match path.parent() {
                    Some(p) => match p.to_str().map(String::from) {
                        Some(pref) => {
                            if pref.is_empty() || prefix.ends_with("..") {
                                if prefix.is_empty() || prefix == "." {
                                    prefix = "..".to_string();
                                } else {
                                    prefix = format!("../{prefix}");
                                }
                            } else {
                                prefix = pref
                            }
                        }
                        None => {
                            tracing::warn!("Failed to convert path to string");
                            return None;
                        }
                    },
                    None => {
                        tracing::warn!("Failed to find parent for path: {:?}", path);
                        return None;
                    }
                }
                res_str = res_str.replacen("../", "", 1);
            }
            Some(format!("{prefix}/{res_str}"))
        } else if child.starts_with("./") {
            Some(child.replacen("./", &format!("{prefix}/"), 1))
        } else if child.starts_with('/') {
            Some(child.to_string())
        } else {
            Some(format!("{prefix}/{child}"))
        }
    }
}

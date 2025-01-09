use crate::prelude::Span;
use crate::time;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::path::PathBuf;
use std::sync::Arc;
use uuid::Uuid;

/// File Encapsulation
#[derive(Default, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct FileSource {
    /// File ID
    #[serde(skip)]
    pub id: Uuid,
    /// File Path
    pub path: String,
    /// File Source
    pub source: Option<String>,
    /// Last File Access Time
    pub access: Option<time::Time>,
    /// An Ordered List of File Dependencies
    pub dependencies: Option<Vec<Arc<FileSource>>>,
}

// Reduce the size of the debug output by not printing the source code and dependencies
impl fmt::Debug for FileSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FileSource")
            .field("id", &self.id)
            .field("path", &self.path)
            .field("source", &"...")
            .field("access", &self.access)
            .field("dependencies", &"...")
            .finish()
    }
}

impl FileSource {
    /// Generates a fully flattened source code for the given `FileSource` and all its dependencies
    ///
    /// ### Examples
    ///
    /// Let's say you have a file, `a.txt` with two dependencies, `b.txt` and `c.txt`,
    /// `fully_flatten()` will generate a source code string with the contents of `b.txt` and
    /// `c.txt` appended to the end of the contents of `a.txt`.
    pub fn fully_flatten(self_ref: Arc<FileSource>) -> (String, Vec<(Arc<FileSource>, Span)>) {
        // First grab the parent file source
        let mut full_source = self_ref.source.clone().unwrap_or_default();
        let span = Span::new(0..full_source.len(), Some(self_ref.clone()));
        let mut relative_positions = vec![(Arc::clone(&self_ref), span)];

        // Then recursively grab source code for dependencies
        if let Some(vfs) = &self_ref.dependencies {
            for fs in vfs {
                let mut flattened = FileSource::fully_flatten(Arc::clone(fs));
                let span = Span::new(full_source.len()..(full_source.len() + flattened.0.len()), Some(fs.clone()));
                full_source.push_str(&flattened.0);
                relative_positions.append(&mut flattened.1);
                relative_positions.push((Arc::clone(fs), span))
            }
        }

        // Return the full source
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

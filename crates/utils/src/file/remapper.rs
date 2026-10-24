use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::io::{BufReader, Read};
use std::path::{Path, PathBuf};

/// A wrapper for dealing with Remappings
#[derive(Debug, Default, Serialize, Deserialize, PartialEq, Eq, Clone)]
pub struct Remapper {
    /// The remappings
    pub remappings: HashMap<String, String>,
    /// The base directory
    pub base_dir: String,
}

impl Remapper {
    /// Extracts remappings from `foundry.toml` and `remappings.txt`, and infers one for each library
    /// in `lib/` that neither defines, as forge does.
    pub fn new(root: impl AsRef<str>) -> Self {
        let mut inner = HashMap::<String, String>::new();

        // Gracefully parse remappings from foundry.toml
        Remapper::from_foundry(root.as_ref(), &mut inner);

        // And from remappings.txt
        Remapper::from_file(root.as_ref(), &mut inner);

        // And infer the rest from the installed libraries
        Remapper::from_libs(root.as_ref(), &mut inner);

        // Return the constructed remappings
        Self { remappings: inner, base_dir: root.as_ref().to_string() }
    }

    /// Helper to break apart a remapping gracefully
    pub fn split(remapping: &str) -> Option<(String, String)> {
        let mut split = remapping.splitn(2, '=');
        match split.next() {
            Some(from) => split.next().map(|to| (from.to_string(), to.to_string())),
            None => None,
        }
    }

    /// Parse foundry toml remappings
    pub fn from_foundry(root: &str, inner: &mut HashMap<String, String>) {
        // Look for a `foundry.toml` file in the current directory.
        let path = Path::new(root).join("foundry.toml");

        match fs::File::open(&path) {
            Ok(f) => {
                // Open the buffered reader and read foundry.toml
                let mut data = String::new();
                let mut br = BufReader::new(f);

                // Gracefully read foundry.toml
                if let Err(e) = br.read_to_string(&mut data) {
                    tracing::warn!(target: "parser", "Failed to read \"foundry.toml\" file contents!\nError: {:?}", e);
                    return;
                }

                // Parse the foundry.toml file as toml
                let toml = match toml::from_str::<toml::Value>(&data) {
                    Ok(t) => t,
                    Err(e) => {
                        tracing::warn!(target: "parser", "\"foundry.toml\" incorrectly formatted: {:?}", e);
                        return;
                    }
                };

                // Parse the toml as a map
                let toml_map = toml.as_table().cloned().unwrap_or_else(toml::map::Map::new);

                // Transform the mappings into profiles
                let profiles = toml_map.iter().filter_map(|p| p.1.as_table()).collect::<Vec<&toml::map::Map<String, toml::Value>>>();
                let unwrapped_profiles = profiles.iter().flat_map(|t| t.values().collect_vec()).collect::<Vec<&toml::Value>>();

                // Extract the inner tables from each profile
                let inner_tables =
                    unwrapped_profiles.iter().filter_map(|t| t.as_table()).collect::<Vec<&toml::map::Map<String, toml::Value>>>();
                let unwrapped_inner_tables = inner_tables
                    .iter()
                    .flat_map(|t| t.into_iter().filter(|m| m.0.eq("remappings")).map(|m| m.1).collect_vec())
                    .collect::<Vec<&toml::Value>>();

                // Extract mappings that are arrays
                let arr_mappings = unwrapped_inner_tables.iter().filter_map(|t| t.as_array()).collect::<Vec<&Vec<toml::Value>>>();
                let unwrapped_mappings = arr_mappings.iter().cloned().flatten().collect::<Vec<&toml::Value>>();

                // Filter the remappings as strings
                let remapping_strings = unwrapped_mappings.iter().filter_map(|t| t.as_str()).collect::<Vec<&str>>();

                // For each remapping string, try to split it and insert it into the remappings
                remapping_strings.iter().for_each(|remapping| {
                    match Remapper::split(remapping) {
                        Some((from, to)) => {
                            inner.insert(from, to);
                        }
                        None => tracing::warn!(target: "parser", "Failed to split remapping using \"=\" at \"{}\" in \"{}\"!", remapping, path.to_string_lossy()),
                    }
                });
            }
            Err(e) => {
                tracing::warn!(target: "parser", "Foundry.toml not found in specified \"{}\"", root);
                tracing::warn!(target: "parser", "{:?}", e);
            }
        }
    }

    /// Infers a remapping for each library in `lib/` without one, as forge does: `name/` to the
    /// library's `src/` folder when it has one, or to the library itself
    pub fn from_libs(root: &str, inner: &mut HashMap<String, String>) {
        let Ok(entries) = fs::read_dir(Path::new(root).join("lib")) else {
            return;
        };
        for entry in entries.filter_map(Result::ok).filter(|entry| entry.path().is_dir()) {
            let name = entry.file_name().to_string_lossy().into_owned();
            if name.starts_with('.') || inner.contains_key(&name) || inner.contains_key(&format!("{name}/")) {
                continue;
            }
            let to = if entry.path().join("src").is_dir() { format!("lib/{name}/src/") } else { format!("lib/{name}/") };
            inner.insert(format!("{name}/"), to);
        }
    }

    /// Get remappings from a remappings.txt file
    pub fn from_file(root: &str, inner: &mut HashMap<String, String>) {
        let mut remappings: HashMap<String, String> = HashMap::new();
        let remappings_file = PathBuf::new().join(root).join("remappings.txt");
        if remappings_file.is_file() {
            let content = fs::read_to_string(remappings_file).map_err(|err| err.to_string()).unwrap();

            let rem_lines = content.lines().collect::<Vec<&str>>();
            let rem = rem_lines.iter().filter(|l| l != &&"").map(|l| l.split_once('=')).collect::<Vec<Option<(&str, &str)>>>();
            rem.iter().for_each(|pair| {
                if let Some((lib, path)) = pair {
                    remappings.insert(lib.to_string(), path.to_string());
                }
            });

            inner.extend(remappings);
        }
    }
}

impl Remapper {
    /// Remaps the start of a path with the remapping of the longest matching prefix, as forge does
    pub fn remap(&self, path: &str) -> Option<String> {
        let (from, to) = self.remappings.iter().filter(|(from, _)| path.starts_with(from.as_str())).max_by_key(|(from, _)| from.len())?;
        tracing::debug!(target: "parser", "found key {} and value {}", from, to);
        Some(format!("{}{to}{}", self.base_dir, &path[from.len()..]))
    }
}

//! ## Artifact
//!
//! The artifacts generated from codegen.

use serde::{Deserialize, Serialize};
use std::{fs, path::Path, sync::Arc};

pub use crate::abi::Abi;
use crate::file::file_source::FileSource;

/// A bytecode source mapping entry
#[derive(Default, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct SourceMapEntry {
    /// Instruction Counter (IC) - the instruction index (0, 1, 2, ...)
    /// This is used to index into the source map array
    pub ic: usize,
    /// Program Counter (PC) - the byte offset in the bytecode where this instruction starts
    /// For example: if PUSH1 0x42 starts at byte 0, PC = 0
    pub pc: usize,
    /// Length of the bytecode segment for this instruction (in bytes)
    /// For example: PUSH1 0x42 = 2 bytes, PUSH0 = 1 byte, SSTORE = 1 byte
    pub bytecode_length: usize,
    /// Start position in the source file (character offset)
    pub source_start: usize,
    /// Length of the source code that generated this instruction (in characters)
    /// This is NOT the bytecode length, but the source text length
    pub source_length: usize,
    /// File ID for multi-file support (0 for main file, 1+ for includes)
    #[serde(default)]
    pub file_id: u32,
}

impl SourceMapEntry {
    /// Generate a Solidity-compatible source map from a vector of SourceMapEntry
    ///
    /// Solidity format: "s:l:f:j" where:
    /// - s = source start position
    /// - l = length in source (end - start)
    /// - f = source file index (0 for single file)
    /// - j = jump type (i/o/- for into/out/regular)
    ///
    /// Multiple entries separated by semicolons, with omitted values if same as previous
    pub fn generate_solidity_source_map(entries: &[SourceMapEntry]) -> String {
        if entries.is_empty() {
            return String::new();
        }

        let mut result = Vec::new();
        let mut prev_start = None;
        let mut prev_length = None;
        let mut prev_file = None;

        for entry in entries {
            let mut parts = Vec::new();

            // First field is the start position in the source file
            // NOT the instruction counter! Solidity source maps use source offset
            if prev_start != Some(entry.source_start) {
                parts.push(entry.source_start.to_string());
                prev_start = Some(entry.source_start);
            } else {
                parts.push(String::new());
            }

            // Length in source
            if prev_length != Some(entry.source_length) {
                parts.push(entry.source_length.to_string());
                prev_length = Some(entry.source_length);
            } else {
                parts.push(String::new());
            }

            // File index for multi-file support
            let file_index = entry.file_id;
            if prev_file != Some(file_index) {
                parts.push(file_index.to_string());
                prev_file = Some(file_index);
            } else {
                parts.push(String::new());
            }

            // Jump type - for now always regular (-), could be enhanced
            parts.push("-".to_string());

            result.push(parts.join(":"));
        }

        result.join(";")
    }
}

/// A Codegen Artifact
#[derive(Default, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Artifact {
    /// The file source
    pub file: Arc<FileSource>,
    /// The deployed bytecode
    pub bytecode: String,
    /// The runtime bytecode
    pub runtime: String,
    /// The abi
    pub abi: Option<Abi>,
    /// Source map for constructor
    pub constructor_map: Option<Vec<SourceMapEntry>>,
    /// Source map for runtime
    pub runtime_map: Option<Vec<SourceMapEntry>>,
}

impl Artifact {
    /// Exports an artifact to a json file
    pub fn export(&self, out: &str) -> Result<(), std::io::Error> {
        let serialized_artifact = serde_json::to_string_pretty(self)?;
        let file_path = Path::new(out);
        if let Some(p) = file_path.parent() {
            tracing::debug!(target: "abi", "Creating directory: \"{:?}\"", p);
            fs::create_dir_all(p)?
        }
        fs::write(file_path, serialized_artifact)
    }
}

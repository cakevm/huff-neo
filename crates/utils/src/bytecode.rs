//! Bytecode Traits
//!
//! Abstract translating state into bytecode.

use crate::ast::span::AstSpan;
use crate::{
    evm_version::EVMVersion,
    prelude::{Statement, TableDefinition},
};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Display},
};

/// A string of Bytes
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bytes(pub String);

/// Intermediate Bytecode Representation
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IRBytes<'a> {
    /// The type of IRByte
    pub ty: IRByteType,
    /// The Span of the IRBytes
    pub span: &'a AstSpan,
}

/// IRBytes Type
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IRByteType {
    /// Bytes
    Bytes(Bytes),
    /// Macro Statement to be expanded
    Statement(Statement),
    /// A Constant to be referenced
    Constant(String),
    /// An Arg Call needs to use the calling macro context.
    /// Macro name and argument name
    ArgCall(String, String),
}

/// Full Intermediate Bytecode Representation
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IRBytecode<'a>(pub Vec<IRBytes<'a>>);

/// ToIRBytecode
///
/// Converts a stateful object to intermediate bytecode
pub trait ToIRBytecode<E> {
    /// Translates `self` to intermediate bytecode representation
    fn to_irbytecode(&self, evm_version: &EVMVersion) -> Result<IRBytecode<'_>, E>;
}

/// Full Bytecode
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bytecode(pub String);

/// ToBytecode
///
/// Converts a stateful object to bytecode
pub trait ToBytecode<'a, E> {
    /// Translates `self` to a bytecode string
    fn to_bytecode(&self) -> Result<Bytecode, E>;
}

impl From<Vec<Bytes>> for Bytecode {
    fn from(b: Vec<Bytes>) -> Self {
        Bytecode(b.iter().fold("".to_string(), |acc, b| format!("{acc}{}", b.0)))
    }
}

/// Result type for [huff_codegen](../../huff_codegen)'s
/// [`recurse_bytecode`](../../huff_codegen/src/lib.rs#recurse_bytecode)
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytecodeRes {
    /// Resulting bytes
    pub bytes: Vec<(usize, Bytes)>,
    /// Jump Indices
    pub label_indices: LabelIndices,
    /// Unmatched Jumps
    pub unmatched_jumps: Jumps,
    /// Table Instances
    pub table_instances: Jumps,
    /// Utilized Tables
    pub utilized_tables: Vec<TableDefinition>,
}

impl Display for BytecodeRes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            r#"BytecodeRes(
            bytes: [{}],
            label_indices: {:?},
            unmatched_jumps: {:?}
            table_instances: {:?}
        )"#,
            self.bytes.iter().fold("".to_string(), |acc, b| format!("{acc}{}", b.0)),
            self.label_indices,
            self.unmatched_jumps,
            self.table_instances
        )
    }
}

/// A Jump
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Jump {
    /// Jump's Label
    pub label: String,
    /// Index of jump within bytecode
    pub bytecode_index: usize,
    /// The Jump Span
    pub span: AstSpan,
    /// The scope depth where the jump was made
    pub scope_depth: usize,
    /// The scope path where the jump was made
    pub scope_path: Vec<String>,
}

/// Type for a vec of `Jump`s
pub type Jumps = Vec<Jump>;

/// Scoped label information
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopedLabel {
    /// The bytecode offset of the label
    pub offset: usize,
    /// The scope depth where the label was defined (0 = top level)
    pub scope_depth: usize,
    /// The macro invocation chain that led to this label
    pub scope_path: Vec<String>,
}

/// Scoped label indices that track labels with their scope information
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopedLabelIndices {
    /// Map of label name to all its definitions with scope info
    labels: BTreeMap<String, Vec<ScopedLabel>>,
}

impl ScopedLabelIndices {
    /// Create a new empty scoped label indices
    pub fn new() -> Self {
        Self { labels: BTreeMap::new() }
    }

    /// Insert a label with scope information
    /// Returns an error if the label already exists in the same scope
    pub fn insert(&mut self, name: String, offset: usize, scope_depth: usize, scope_path: Vec<String>) -> Result<(), String> {
        let entry = self.labels.entry(name.clone()).or_default();

        // Check if label already exists at the same scope depth
        for existing in entry.iter() {
            if existing.scope_depth == scope_depth && existing.scope_path == scope_path {
                return Err(format!("Duplicate label '{}' in the same scope", name));
            }
        }

        entry.push(ScopedLabel { offset, scope_depth, scope_path });
        Ok(())
    }

    /// Get the label offset for a given name, considering scope
    /// Returns the label from the deepest matching scope
    pub fn get(&self, name: &str, current_scope_depth: usize, current_scope_path: &[String]) -> Option<usize> {
        self.labels.get(name).and_then(|labels| {
            // Find the label with the deepest scope that is still accessible
            labels
                .iter()
                .filter(|label| {
                    // Label is accessible if it's at the same depth or shallower
                    // and the scope path matches up to the label's depth
                    label.scope_depth <= current_scope_depth
                        && label.scope_path.len() <= current_scope_path.len()
                        && label.scope_path.iter().zip(current_scope_path.iter()).all(|(a, b)| a == b)
                })
                .max_by_key(|label| label.scope_depth)
                .map(|label| label.offset)
        })
    }

    /// Get any label offset for a given name, regardless of scope
    /// Used for jump tables which may reference labels in any scope
    /// Returns the first matching label if there are multiple
    pub fn get_any(&self, name: &str) -> Option<usize> {
        self.labels.get(name).and_then(|labels| labels.first().map(|label| label.offset))
    }

    /// Extend this indices with another set of indices
    pub fn extend(&mut self, other: ScopedLabelIndices) {
        for (name, labels) in other.labels {
            self.labels.entry(name).or_default().extend(labels);
        }
    }

    /// Check if the indices are empty
    pub fn is_empty(&self) -> bool {
        self.labels.is_empty()
    }

    /// Iterate over all labels
    pub fn iter(&self) -> impl Iterator<Item = (&String, &Vec<ScopedLabel>)> {
        self.labels.iter()
    }
}

/// Type to map `Jump` labels to their bytecode indices (for backward compatibility)
pub type LabelIndices = ScopedLabelIndices;

/// Typw to map circular_codesize labels to their bytecode indices
pub type CircularCodeSizeIndices = BTreeSet<(String, usize)>;

/// Type for a map of bytecode indexes to `Jumps`. Represents a Jump Table.
pub type JumpTable = BTreeMap<usize, Jumps>;

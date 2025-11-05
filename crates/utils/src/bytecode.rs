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

/// Bytecode with different types for placeholders
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Bytes {
    /// Raw bytecode - ready to use
    Raw(String),
    /// Jump placeholder - "xxxx" needs label resolution
    JumpPlaceholder(String),
    /// Circular codesize placeholder - "cccc" for self-referential __codesize
    CircularCodesizePlaceholder(String),
    /// Dynamic constructor arg placeholder
    DynConstructorArgPlaceholder(String),
}

impl Bytes {
    /// Get the underlying hex string
    pub fn as_str(&self) -> &str {
        match self {
            Bytes::Raw(s) | Bytes::JumpPlaceholder(s) | Bytes::CircularCodesizePlaceholder(s) | Bytes::DynConstructorArgPlaceholder(s) => s,
        }
    }

    /// Get the length of the hex string
    pub fn len(&self) -> usize {
        self.as_str().len()
    }

    /// Check if the hex string is empty
    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }
}

impl Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// A segment of bytecode with its offset
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytecodeSegment {
    /// The bytecode offset where this segment starts
    pub offset: usize,
    /// The hex-encoded bytecode string
    pub bytes: Bytes,
}

impl BytecodeSegment {
    /// Create a new bytecode segment
    pub fn new(offset: usize, bytes: Bytes) -> Self {
        Self { offset, bytes }
    }
}

/// Collection of bytecode segments with helper methods
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytecodeSegments {
    segments: Vec<BytecodeSegment>,
}

impl BytecodeSegments {
    /// Create a new empty collection
    pub fn new() -> Self {
        Self { segments: Vec::new() }
    }

    /// Push a bytecode segment
    pub fn push(&mut self, segment: BytecodeSegment) {
        self.segments.push(segment);
    }

    /// Push a bytecode segment with an offset (without scope information)
    pub fn push_with_offset(&mut self, offset: usize, bytes: Bytes) {
        self.segments.push(BytecodeSegment::new(offset, bytes));
    }

    /// Extend with another collection
    pub fn extend(&mut self, other: BytecodeSegments) {
        self.segments.extend(other.segments);
    }

    /// Get an iterator over the segments
    pub fn iter(&self) -> impl Iterator<Item = &BytecodeSegment> {
        self.segments.iter()
    }

    /// Get the number of segments
    pub fn len(&self) -> usize {
        self.segments.len()
    }

    /// Check if the collection is empty
    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }
}

impl IntoIterator for BytecodeSegments {
    type Item = BytecodeSegment;
    type IntoIter = std::vec::IntoIter<BytecodeSegment>;

    fn into_iter(self) -> Self::IntoIter {
        self.segments.into_iter()
    }
}

impl<'a> IntoIterator for &'a BytecodeSegments {
    type Item = &'a BytecodeSegment;
    type IntoIter = std::slice::Iter<'a, BytecodeSegment>;

    fn into_iter(self) -> Self::IntoIter {
        self.segments.iter()
    }
}

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
        Bytecode(b.iter().fold("".to_string(), |acc, b| format!("{acc}{}", b.as_str())))
    }
}

/// Result type for [huff_codegen](../../huff_codegen)'s
/// [`recurse_bytecode`](../../huff_codegen/src/lib.rs#recurse_bytecode)
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytecodeRes {
    /// Resulting bytes
    pub bytes: BytecodeSegments,
    /// Source spans for each bytecode segment (parallel to bytes vec)
    pub spans: Vec<Option<(usize, usize)>>,
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
            self.bytes.iter().fold("".to_string(), |acc, b| format!("{acc}{}", b.bytes.as_str())),
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
    /// Returns an error if duplicate labels are found in sibling scopes when cross-referencing
    pub fn get(&self, name: &str, current_scope_depth: usize, current_scope_path: &[String]) -> Result<Option<usize>, String> {
        self.labels.get(name).map_or(Ok(None), |labels| {
            // First, try to find a label in the current scope or parent scopes
            let in_scope = labels
                .iter()
                .filter(|label| {
                    // Label is accessible if it's at the same depth or shallower
                    // and the scope path matches up to the label's depth
                    label.scope_depth <= current_scope_depth
                        && label.scope_path.len() <= current_scope_path.len()
                        && label.scope_path.iter().zip(current_scope_path.iter()).all(|(a, b)| a == b)
                })
                .max_by_key(|label| label.scope_depth)
                .map(|label| label.offset);

            if in_scope.is_some() {
                return Ok(in_scope);
            }

            // If not found in current/parent scopes, search in child scopes
            // This allows accessing labels defined in macros invoked from the current scope
            let child_labels: Vec<&ScopedLabel> = labels
                .iter()
                .filter(|label| {
                    // Check if this label is in a child scope (starts with our scope path)
                    label.scope_path.len() > current_scope_path.len()
                        && current_scope_path.iter().zip(label.scope_path.iter()).all(|(a, b)| a == b)
                })
                .collect();

            if !child_labels.is_empty() {
                // Check for duplicates in child scopes
                if child_labels.len() > 1 {
                    // Multiple children define this label - use the first one (depth-first order)
                    // This matches the order of macro expansion
                }
                return Ok(child_labels.first().map(|label| label.offset));
            }

            // If not found in current/parent/child scopes, search in sibling scopes
            // This allows accessing labels defined in sibling macros invoked from the same parent
            // We need to check siblings at all levels up the tree
            for depth in (0..current_scope_path.len()).rev() {
                // Get the scope at this depth
                let search_scope = &current_scope_path[..depth + 1];

                // Find all labels in cousin/sibling scopes that share this common ancestor
                let cousin_labels: Vec<&ScopedLabel> = labels
                    .iter()
                    .filter(|label| {
                        // Must have at least the depth we're checking
                        if label.scope_path.len() <= depth {
                            return false;
                        }

                        // Must share the common ancestor path
                        if label.scope_path[..depth] != search_scope[..depth] {
                            return false;
                        }

                        // If we're at the immediate parent level, exclude our own path
                        if depth == current_scope_path.len() - 1 {
                            // Same depth siblings
                            label.scope_path.len() == current_scope_path.len() && label.scope_path != current_scope_path
                        } else {
                            // Different branch from this ancestor - must diverge after depth
                            if label.scope_path.len() > depth {
                                // The path diverges at depth+1 (different child of common ancestor)
                                label.scope_path.get(depth) != current_scope_path.get(depth)
                            } else {
                                false
                            }
                        }
                    })
                    .collect();

                if !cousin_labels.is_empty() {
                    // Check for duplicates across cousins only when we're doing cross-reference
                    if cousin_labels.len() > 1 {
                        // Multiple cousins define this label - check if they're in different branches
                        let unique_scopes: std::collections::HashSet<_> = cousin_labels.iter().map(|label| &label.scope_path).collect();

                        if unique_scopes.len() > 1 {
                            // Labels are in different cousin scopes - ambiguous reference
                            return Err(format!("DuplicateLabelAcrossSiblings:{}", name));
                        }
                    }

                    return Ok(cousin_labels.first().map(|label| label.offset));
                }
            }

            Ok(None)
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
        for (name, new_labels) in other.labels {
            self.labels.entry(name).or_default().extend(new_labels);
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

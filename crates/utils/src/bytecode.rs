//! Bytecode Traits
//!
//! Abstract translating state into bytecode.

use crate::ast::span::AstSpan;
use crate::scope::ScopeId;
use crate::{
    evm_version::EVMVersion,
    prelude::{Statement, TableDefinition},
};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Display},
};

/// Push opcode for jump instructions
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PushOpcode {
    /// PUSH1 opcode (0x60) - pushes 1 byte onto stack
    Push1,
    /// PUSH2 opcode (0x61) - pushes 2 bytes onto stack
    Push2,
}

impl PushOpcode {
    /// Convert to hex string representation
    pub fn to_hex(&self) -> &'static str {
        match self {
            PushOpcode::Push1 => "60",
            PushOpcode::Push2 => "61",
        }
    }

    /// Get the number of bytes this push opcode accepts (not including the opcode itself)
    pub fn byte_size(&self) -> usize {
        match self {
            PushOpcode::Push1 => 1,
            PushOpcode::Push2 => 2,
        }
    }

    /// Check if a value can be represented by this push opcode
    pub fn can_represent(&self, value: usize) -> bool {
        match self {
            PushOpcode::Push1 => value <= 0xFF,
            PushOpcode::Push2 => value <= 0xFFFF,
        }
    }

    /// Get the optimal push opcode for a given value
    pub fn optimal_for(value: usize) -> Self {
        if value <= 0xFF { PushOpcode::Push1 } else { PushOpcode::Push2 }
    }

    /// Parse from hex string (for tests and compatibility)
    pub fn from_hex(s: &str) -> Option<Self> {
        match s {
            "60" => Some(PushOpcode::Push1),
            "61" => Some(PushOpcode::Push2),
            _ => None,
        }
    }
}

/// Data for a jump placeholder that needs label resolution
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct JumpPlaceholderData {
    /// The label name this jump references
    pub label: String,
    /// The push opcode to use for the jump destination
    pub push_opcode: PushOpcode,
    /// The opcode suffix (e.g., "565b" for Jump+Jumpdest, or "" for simple jumps)
    pub suffix: String,
    /// The resolved target offset (None if not yet resolved)
    pub target_offset: Option<usize>,
}

impl JumpPlaceholderData {
    /// Create a new unresolved jump placeholder
    pub fn new(label: String, push_opcode: PushOpcode, suffix: String) -> Self {
        Self { label, push_opcode, suffix, target_offset: None }
    }

    /// Create a jump placeholder with a resolved target
    pub fn with_target(label: String, push_opcode: PushOpcode, suffix: String, target_offset: usize) -> Self {
        Self { label, push_opcode, suffix, target_offset: Some(target_offset) }
    }

    /// Check if this jump has been resolved
    pub fn is_resolved(&self) -> bool {
        self.target_offset.is_some()
    }

    /// Resolve this jump to a target offset
    pub fn resolve(&mut self, target_offset: usize) {
        self.target_offset = Some(target_offset);
    }

    /// Generate the hex string representation
    /// If resolved, returns the actual bytecode. If unresolved, returns placeholder with "xxxx"
    pub fn to_hex(&self) -> String {
        let value_hex = if let Some(offset) = self.target_offset {
            // Resolved: format as actual hex value
            match self.push_opcode {
                PushOpcode::Push1 => format!("{:02x}", offset),
                PushOpcode::Push2 => format!("{:04x}", offset),
            }
        } else {
            // Unresolved: use placeholder
            "x".repeat(self.push_opcode.byte_size() * 2)
        };

        format!("{}{}{}", self.push_opcode.to_hex(), value_hex, self.suffix)
    }

    /// Legacy method for compatibility - same as to_hex()
    pub fn placeholder_string(&self) -> String {
        self.to_hex()
    }
}

/// Data for a circular codesize placeholder
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct CircularCodesizePlaceholderData {
    /// The macro name that contains this circular reference
    pub macro_name: String,
    /// The push opcode to use (PUSH1 or PUSH2, determined during resolution)
    pub push_opcode: PushOpcode,
    /// The resolved macro size (None if not yet resolved)
    pub resolved_value: Option<usize>,
}

impl CircularCodesizePlaceholderData {
    /// Create a new unresolved circular codesize placeholder
    /// Starts with PUSH1 optimistically, may grow to PUSH2 during resolution
    pub fn new(macro_name: String) -> Self {
        Self { macro_name, push_opcode: PushOpcode::Push1, resolved_value: None }
    }

    /// Create a circular codesize placeholder with a resolved value
    pub fn with_value(macro_name: String, size: usize) -> Self {
        let push_opcode = PushOpcode::optimal_for(size);
        Self { macro_name, push_opcode, resolved_value: Some(size) }
    }

    /// Check if this codesize has been resolved
    pub fn is_resolved(&self) -> bool {
        self.resolved_value.is_some()
    }

    /// Resolve this codesize to a specific size
    /// Returns true if the push opcode grew from PUSH1 to PUSH2
    pub fn resolve(&mut self, size: usize) -> bool {
        let old_opcode = self.push_opcode;
        self.push_opcode = PushOpcode::optimal_for(size);
        self.resolved_value = Some(size);

        // Return true if we grew from PUSH1 to PUSH2
        matches!((old_opcode, self.push_opcode), (PushOpcode::Push1, PushOpcode::Push2))
    }

    /// Check if this placeholder could grow during resolution
    pub fn can_grow(&self) -> bool {
        matches!(self.push_opcode, PushOpcode::Push1)
    }

    /// Generate the hex string representation
    /// If resolved, returns the actual bytecode. If unresolved, returns placeholder with "cccc"
    pub fn to_hex(&self) -> String {
        if let Some(size) = self.resolved_value {
            // Resolved: format as actual push instruction
            let value_hex = match self.push_opcode {
                PushOpcode::Push1 => format!("{:02x}", size),
                PushOpcode::Push2 => format!("{:04x}", size),
            };
            format!("{}{}", self.push_opcode.to_hex(), value_hex)
        } else {
            // Unresolved: use "cccc" placeholder (2 bytes = PUSH1 size)
            "cccc".to_string()
        }
    }

    /// Legacy method for compatibility - same as to_hex()
    pub fn placeholder_string(&self) -> String {
        self.to_hex()
    }
}

/// Data for a dynamic constructor argument placeholder
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DynConstructorArgPlaceholderData {
    /// The constructor argument name
    pub arg_name: String,
    /// The destination offset (as a padded hex string)
    pub dest_offset: String,
}

impl DynConstructorArgPlaceholderData {
    /// Create a new dynamic constructor arg placeholder
    pub fn new(arg_name: String, dest_offset: String) -> Self {
        Self { arg_name, dest_offset }
    }

    /// Generate the placeholder string
    /// Format: 28 'x' characters + arg_name + dest_offset (pre-padded)
    pub fn placeholder_string(&self) -> String {
        format!("{}{}{}", "xx".repeat(14), self.arg_name, self.dest_offset)
    }
}

/// Bytecode with different types for placeholders
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Bytes {
    /// Raw bytecode - ready to use
    Raw(String),
    /// Jump placeholder - "xxxx" needs label resolution
    JumpPlaceholder(JumpPlaceholderData),
    /// Circular codesize placeholder - "cccc" for self-referential __codesize
    CircularCodesizePlaceholder(CircularCodesizePlaceholderData),
    /// Dynamic constructor arg placeholder
    DynConstructorArgPlaceholder(DynConstructorArgPlaceholderData),
}

impl Bytes {
    /// Get the underlying hex string (generates it for placeholders)
    pub fn as_str(&self) -> String {
        match self {
            Bytes::Raw(s) => s.clone(),
            Bytes::JumpPlaceholder(data) => data.to_hex(),
            Bytes::CircularCodesizePlaceholder(data) => data.to_hex(),
            Bytes::DynConstructorArgPlaceholder(data) => data.placeholder_string(),
        }
    }

    /// Convert to hex representation (same as as_str but more explicit name)
    pub fn to_hex(&self) -> String {
        self.as_str()
    }

    /// Get the length in bytes (not hex characters)
    /// Each pair of hex characters represents one byte
    pub fn len(&self) -> usize {
        match self {
            Bytes::Raw(s) => s.len() / 2,
            Bytes::JumpPlaceholder(data) => {
                // 1 byte for opcode + N bytes for value + suffix bytes
                1 + data.push_opcode.byte_size() + (data.suffix.len() / 2)
            }
            Bytes::CircularCodesizePlaceholder(data) => {
                // 1 byte for opcode + N bytes for value
                1 + data.push_opcode.byte_size()
            }
            Bytes::DynConstructorArgPlaceholder(data) => {
                // 28 'x' characters = 14 bytes + arg_name + dest_offset
                14 + (data.arg_name.len() / 2) + (data.dest_offset.len() / 2)
            }
        }
    }

    /// Check if the bytecode is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
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

    /// Get a mutable iterator over the segments
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut BytecodeSegment> {
        self.segments.iter_mut()
    }

    /// Get the number of segments
    pub fn len(&self) -> usize {
        self.segments.len()
    }

    /// Check if the collection is empty
    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    /// Calculate the cumulative byte offsets for each segment
    /// Returns a vector where each element is the byte offset of the corresponding segment
    pub fn calculate_offsets(&self) -> Vec<usize> {
        let mut offsets = Vec::with_capacity(self.segments.len());
        let mut current_offset = 0;

        for segment in &self.segments {
            offsets.push(current_offset);
            current_offset += segment.bytes.len();
        }

        offsets
    }

    /// Calculate the total byte size of all segments
    pub fn total_byte_size(&self) -> usize {
        self.segments.iter().map(|seg| seg.bytes.len()).sum()
    }

    /// Find all jump placeholders with their byte offsets
    /// Returns a vector of (offset, jump_data) tuples
    pub fn find_jump_placeholders(&self) -> Vec<(usize, &JumpPlaceholderData)> {
        let mut jumps = Vec::new();
        let mut offset = 0;

        for segment in &self.segments {
            if let Bytes::JumpPlaceholder(ref data) = segment.bytes {
                jumps.push((offset, data));
            }
            offset += segment.bytes.len();
        }

        jumps
    }

    /// Resolve all jump placeholders using the provided jump table and label indices
    /// Returns a vector of unmatched jumps that couldn't be resolved
    pub fn resolve_jumps(&mut self, jump_table: &JumpTable, label_indices: &LabelIndices) -> Result<Vec<Jump>, LabelError> {
        let mut unmatched_jumps = Vec::new();

        for segment in &mut self.segments {
            let code_index = segment.offset;

            // Check if a jump table exists at this segment's offset
            if let Some(jt) = jump_table.get(&code_index) {
                // Try to resolve the jump placeholder
                if let Bytes::JumpPlaceholder(ref mut data) = segment.bytes {
                    // Get the first jump from the table (there should only be one per segment)
                    if let Some(jump) = jt.first() {
                        // Verify the label names match (debugging/consistency check)
                        // This should never happen - it indicates a compiler bug
                        assert_eq!(
                            data.label, jump.label,
                            "Label mismatch at offset {:#x}: placeholder has \"{}\", jump table has \"{}\"",
                            code_index, data.label, jump.label
                        );

                        // Look up the label's target offset
                        match label_indices.get(jump.label.as_str(), &jump.scope_id) {
                            Ok(Some(target_offset)) => {
                                // Verify the offset fits in the chosen push opcode
                                if !data.push_opcode.can_represent(target_offset) {
                                    return Err(LabelError::JumpTargetTooLarge {
                                        label: jump.label.clone(),
                                        target: target_offset,
                                        opcode: format!("{:?}", data.push_opcode),
                                    });
                                }
                                // Resolve the jump
                                data.resolve(target_offset);
                            }
                            Ok(None) => {
                                // Unmatched jump - add to list
                                unmatched_jumps.push(Jump {
                                    label: jump.label.clone(),
                                    bytecode_index: code_index,
                                    span: jump.span.clone(),
                                    scope_id: jump.scope_id.clone(),
                                });
                            }
                            Err(err_msg) => {
                                return Err(err_msg);
                            }
                        }
                    }
                }
            }
        }

        Ok(unmatched_jumps)
    }

    /// Resolve all circular codesize placeholders with calculated sizes
    /// Returns a vector of (offset, growth_amount) tuples for placeholders that grew from PUSH1 to PUSH2
    pub fn resolve_circular_codesize(
        &mut self,
        circular_codesize_invocations: &CircularCodeSizeIndices,
        _macro_name: &str,
        extended_length: usize,
    ) -> Vec<(usize, usize)> {
        let mut growth_offsets = Vec::new();

        // Use the extended_length passed from caller which accounts for all placeholder growth
        tracing::debug!(
            target: "codegen",
            "Resolving circular codesize with extended_length: {}",
            extended_length
        );

        for segment in &mut self.segments {
            // Check if this segment is a circular codesize placeholder at a registered invocation point
            if let Some((placeholder_macro, _offset)) = circular_codesize_invocations.iter().find(|(_, offset)| *offset == segment.offset)
                && let Bytes::CircularCodesizePlaceholder(ref mut data) = segment.bytes
            {
                // Verify the macro names match
                if &data.macro_name != placeholder_macro {
                    tracing::warn!(
                        target: "codegen",
                        "Circular codesize macro mismatch at offset {:#x}: placeholder has \"{}\", expected \"{}\"",
                        segment.offset, data.macro_name, placeholder_macro
                    );
                    continue;
                }

                // Resolve with the extended_length that accounts for all placeholder growth
                let grew = data.resolve(extended_length);
                if grew {
                    // Track this offset for later adjustment
                    growth_offsets.push((segment.offset, 1)); // Grew by 1 byte (PUSH1→PUSH2)
                    tracing::debug!(
                        target: "codegen",
                        "Circular codesize placeholder grew from PUSH1 to PUSH2 at offset {:#x}",
                        segment.offset
                    );
                }
            }
        }

        growth_offsets
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
    Statement(Box<Statement>),
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Jump {
    /// Jump's Label
    pub label: String,
    /// Index of jump within bytecode
    pub bytecode_index: usize,
    /// The Jump Span
    pub span: AstSpan,
    /// The scope where the jump was made
    pub scope_id: ScopeId,
}

impl Default for Jump {
    fn default() -> Self {
        Self { label: String::new(), bytecode_index: 0, span: AstSpan::default(), scope_id: ScopeId::new(vec![], 0) }
    }
}

/// Type for a vec of `Jump`s
pub type Jumps = Vec<Jump>;

/// Scoped label information
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopedLabel {
    /// The bytecode offset of the label
    pub offset: usize,
    /// The scope where the label was defined
    pub scope_id: ScopeId,
}

/// Error type for label operations
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum LabelError {
    /// A label was defined multiple times in the same scope
    #[error("Duplicate label '{0}' in the same scope")]
    DuplicateLabelInScope(String),
    /// A label is defined in multiple sibling scopes, creating ambiguity
    #[error("Duplicate label '{0}' across sibling scopes")]
    DuplicateLabelAcrossSiblings(String),
    /// A jump target is too large for the specified opcode
    #[error("Jump target {target:#x} too large for {opcode} in label \"{label}\"")]
    JumpTargetTooLarge {
        /// The label name
        label: String,
        /// The target offset that was too large
        target: usize,
        /// The opcode that couldn't represent the target
        opcode: String,
    },
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
    pub fn insert(&mut self, name: String, offset: usize, scope_id: &ScopeId) -> Result<(), LabelError> {
        let entry = self.labels.entry(name.clone()).or_default();

        // Check if label already exists at the same scope
        for existing in entry.iter() {
            if existing.scope_id == *scope_id {
                return Err(LabelError::DuplicateLabelInScope(name));
            }
        }

        entry.push(ScopedLabel { offset, scope_id: scope_id.clone() });
        Ok(())
    }

    /// Get the label offset for a given name, considering scope
    /// Returns the label from the deepest matching scope
    /// Returns an error if duplicate labels are found in sibling scopes when cross-referencing
    pub fn get(&self, name: &str, current_scope: &ScopeId) -> Result<Option<usize>, LabelError> {
        let current_scope_depth = current_scope.depth;
        let current_scope_path = &current_scope.path;

        self.labels.get(name).map_or(Ok(None), |labels| {
            // First, try to find a label in the current scope or parent scopes
            let in_scope = labels
                .iter()
                .filter(|label| {
                    // Label is accessible if it's at the same depth or shallower
                    // and the scope path matches up to the label's depth
                    label.scope_id.depth <= current_scope_depth
                        && label.scope_id.path.len() <= current_scope_path.len()
                        && label.scope_id.path.iter().zip(current_scope_path.iter()).all(|(a, b)| a == b)
                })
                .max_by_key(|label| label.scope_id.depth)
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
                    label.scope_id.path.len() > current_scope_path.len()
                        && current_scope_path.iter().zip(label.scope_id.path.iter()).all(|(a, b)| a == b)
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
                        if label.scope_id.path.len() <= depth {
                            return false;
                        }

                        // Must share the common ancestor path
                        if label.scope_id.path[..depth] != search_scope[..depth] {
                            return false;
                        }

                        // If we're at the immediate parent level, exclude our own path
                        if depth == current_scope_path.len() - 1 {
                            // Same depth siblings
                            label.scope_id.path.len() == current_scope_path.len() && label.scope_id.path != *current_scope_path
                        } else {
                            // Different branch from this ancestor - must diverge after depth
                            if label.scope_id.path.len() > depth {
                                // The path diverges at depth+1 (different child of common ancestor)
                                label.scope_id.path.get(depth) != current_scope_path.get(depth)
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
                        let unique_scopes: std::collections::HashSet<_> = cousin_labels.iter().map(|label| &label.scope_id.path).collect();

                        if unique_scopes.len() > 1 {
                            // Labels are in different cousin scopes - ambiguous reference
                            return Err(LabelError::DuplicateLabelAcrossSiblings(name.to_string()));
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

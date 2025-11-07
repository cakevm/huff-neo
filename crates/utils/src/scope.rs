//! Scope management for macro compilation.
//!
//! Tracks macro call stack context to ensure labels and arguments resolve correctly
//! during bytecode generation.
//!
//! # Core Concepts
//!
//! ## ScopeId
//! Uniquely identifies where a label is defined or where a jump should resolve to.
//! Prevents label collisions between different macro invocations.
//!
//! ## ScopeManager
//! Tracks the current macro call stack. Provides scope identifiers for label operations
//! and access to the call stack for argument resolution.

use crate::prelude::{MacroDefinition, MacroInvocation};

/// A single entry in the macro call stack.
///
/// Contains the macro definition, bytecode offset, and arguments passed at invocation.
#[derive(Debug, Clone)]
pub struct ScopeFrame<'a> {
    /// The macro definition being executed
    pub macro_def: &'a MacroDefinition,
    /// The bytecode offset where this macro was invoked
    pub offset: usize,
    /// The invocation details (arguments passed), None for root macro
    pub invocation: Option<MacroInvocation>,
}

/// Uniquely identifies a scope for label storage and resolution.
///
/// Combines a path of macro names with call depth to distinguish between different
/// invocations of the same macro, preventing label name collisions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ScopeId {
    /// Path of macro names with invocation offsets, e.g., ["MAIN", "M1_5", "M2_12"]
    pub path: Vec<String>,
    /// Depth in the macro call stack (0 for MAIN)
    pub depth: usize,
}

impl ScopeId {
    /// Creates a new scope identifier with the given path and depth.
    pub fn new(path: Vec<String>, depth: usize) -> Self {
        Self { path, depth }
    }

    /// Returns the scope identifier for the calling context, or None if already at root.
    pub fn parent(&self) -> Option<ScopeId> {
        if self.depth == 0 { None } else { Some(ScopeId { path: self.path[..self.path.len() - 1].to_vec(), depth: self.depth - 1 }) }
    }
}

/// Tracks the macro call stack during compilation.
///
/// Provides scope identifiers for label operations and exposes the call stack
/// for argument resolution.
pub struct ScopeManager<'a> {
    /// Stack of scope frames from root to current
    pub frames: Vec<ScopeFrame<'a>>,
}

impl<'a> ScopeManager<'a> {
    /// Creates a new empty scope manager.
    pub fn new() -> Self {
        Self { frames: Vec::new() }
    }

    /// Returns the scope identifier for the currently executing macro.
    ///
    /// Used when defining labels or creating jumps within the current macro body.
    pub fn current_scope(&self) -> ScopeId {
        if self.frames.is_empty() {
            return ScopeId::new(vec![], 0);
        }
        self.build_scope_id(self.frames.len() - 1)
    }

    /// Returns the scope identifier for the calling macro.
    ///
    /// Used when resolving label arguments that should be looked up in the caller's scope.
    /// Returns the root scope if called from the top level.
    pub fn parent_scope(&self) -> ScopeId {
        if self.frames.len() <= 1 {
            return ScopeId::new(vec![], 0);
        }
        self.build_scope_id(self.frames.len() - 2)
    }

    /// Enters a new macro context.
    ///
    /// Adds the macro to the call stack at the given bytecode offset.
    /// Use for root macros (MAIN/CONSTRUCTOR) without invocation details.
    pub fn push_macro(&mut self, macro_def: &'a MacroDefinition, offset: usize) {
        self.frames.push(ScopeFrame { macro_def, offset, invocation: None });
    }

    /// Enters a new macro context with invocation arguments.
    ///
    /// Like `push_macro`, but includes the invocation details for argument resolution.
    pub fn push_macro_with_invocation(&mut self, macro_def: &'a MacroDefinition, offset: usize, invocation: MacroInvocation) {
        self.frames.push(ScopeFrame { macro_def, offset, invocation: Some(invocation) });
    }

    /// Exits the current macro context.
    ///
    /// Removes the top frame from the call stack and returns the macro definition.
    pub fn pop_macro(&mut self) -> Option<&'a MacroDefinition> {
        self.frames.pop().map(|frame| frame.macro_def)
    }

    /// Returns the macro definition at the top of the call stack, or None if empty.
    pub fn current_macro(&self) -> Option<&'a MacroDefinition> {
        self.frames.last().map(|frame| frame.macro_def)
    }

    /// Returns all macro definitions from root to current as a vector.
    pub fn macro_stack(&self) -> Vec<&'a MacroDefinition> {
        self.frames.iter().map(|frame| frame.macro_def).collect()
    }

    /// Returns the macro definition at the specified call stack depth, or None if out of bounds.
    pub fn macro_at_depth(&self, depth: usize) -> Option<&'a MacroDefinition> {
        self.frames.get(depth).map(|frame| frame.macro_def)
    }

    /// Returns the number of frames in the call stack.
    pub fn depth(&self) -> usize {
        self.frames.len()
    }

    /// Returns an iterator over (offset, invocation) pairs for all frames.
    ///
    /// Invocation is None for root macros.
    pub fn invocation_stack(&self) -> impl Iterator<Item = (usize, Option<&MacroInvocation>)> + '_ {
        self.frames.iter().map(|frame| (frame.offset, frame.invocation.as_ref()))
    }

    /// Returns (offset, invocation) pairs for all frames with invocation details.
    ///
    /// Root macros without invocations are excluded from the result.
    pub fn invocation_vec(&self) -> Vec<(usize, &MacroInvocation)> {
        self.frames.iter().filter_map(|frame| frame.invocation.as_ref().map(|inv| (frame.offset, inv))).collect()
    }

    /// Returns (offset, invocation) pairs up to the specified depth.
    ///
    /// Used to get the invocation context when resolving arguments for a macro at `target_depth`.
    pub fn invocation_vec_up_to_depth(&self, target_depth: usize) -> Vec<(usize, &MacroInvocation)> {
        self.frames.iter().take(target_depth).filter_map(|frame| frame.invocation.as_ref().map(|inv| (frame.offset, inv))).collect()
    }

    /// Returns the invocation details for the current macro, or None if unavailable.
    pub fn current_invocation(&self) -> Option<&MacroInvocation> {
        self.frames.last().and_then(|frame| frame.invocation.as_ref())
    }

    /// Returns the bytecode offset and invocation details for the current macro.
    pub fn current_invocation_with_offset(&self) -> Option<(usize, Option<&MacroInvocation>)> {
        self.frames.last().map(|frame| (frame.offset, frame.invocation.as_ref()))
    }

    /// Builds a scope ID at a specific depth in the call stack.
    ///
    /// # Arguments
    /// * `target_depth` - The depth to build the scope ID for (0 = root)
    ///
    /// # Panics
    /// Panics if `target_depth` is greater than the current stack depth.
    fn build_scope_id(&self, target_depth: usize) -> ScopeId {
        assert!(target_depth < self.frames.len(), "target_depth {} exceeds stack depth {}", target_depth, self.frames.len());

        let path = if target_depth == 0 {
            // Root scope: just the macro name, no offset
            vec![self.frames[0].macro_def.name.clone()]
        } else {
            // Build path up to and including target_depth
            let mut path = Vec::with_capacity(target_depth + 1);

            // First element has no offset
            path.push(self.frames[0].macro_def.name.clone());

            // Subsequent elements include invocation offset for uniqueness
            for i in 1..=target_depth {
                let frame = &self.frames[i];
                path.push(format!("{}_{}", frame.macro_def.name, frame.offset));
            }

            path
        };

        ScopeId::new(path, target_depth)
    }
}

impl<'a> Default for ScopeManager<'a> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::AstSpan;

    fn make_test_macro(name: &str) -> MacroDefinition {
        MacroDefinition {
            name: name.to_string(),
            decorator: None,
            parameters: Vec::new(),
            statements: Vec::new(),
            takes: 0,
            returns: 0,
            outlined: false,
            test: false,
            span: AstSpan::default(),
        }
    }

    #[test]
    fn test_empty_scope_manager() {
        let mgr = ScopeManager::new();
        assert_eq!(mgr.depth(), 0);
        assert_eq!(mgr.current_scope(), ScopeId::new(vec![], 0));
        assert_eq!(mgr.parent_scope(), ScopeId::new(vec![], 0));
    }

    #[test]
    fn test_single_macro() {
        let main = make_test_macro("MAIN");
        let mut mgr = ScopeManager::new();
        mgr.push_macro(&main, 0);

        assert_eq!(mgr.depth(), 1);
        assert_eq!(mgr.current_scope(), ScopeId::new(vec!["MAIN".to_string()], 0));
        assert_eq!(mgr.parent_scope(), ScopeId::new(vec![], 0));
    }

    #[test]
    fn test_two_level_nesting() {
        let main = make_test_macro("MAIN");
        let m1 = make_test_macro("M1");

        let mut mgr = ScopeManager::new();
        mgr.push_macro(&main, 0);
        mgr.push_macro(&m1, 5);

        assert_eq!(mgr.depth(), 2);
        assert_eq!(mgr.current_scope(), ScopeId::new(vec!["MAIN".to_string(), "M1_5".to_string()], 1));
        assert_eq!(mgr.parent_scope(), ScopeId::new(vec!["MAIN".to_string()], 0));
    }

    #[test]
    fn test_three_level_nesting() {
        let main = make_test_macro("MAIN");
        let m1 = make_test_macro("M1");
        let m2 = make_test_macro("M2");

        let mut mgr = ScopeManager::new();
        mgr.push_macro(&main, 0);
        mgr.push_macro(&m1, 5);
        mgr.push_macro(&m2, 12);

        assert_eq!(mgr.depth(), 3);
        assert_eq!(mgr.current_scope(), ScopeId::new(vec!["MAIN".to_string(), "M1_5".to_string(), "M2_12".to_string()], 2));
        assert_eq!(mgr.parent_scope(), ScopeId::new(vec!["MAIN".to_string(), "M1_5".to_string()], 1));
    }

    #[test]
    fn test_push_pop() {
        let main = make_test_macro("MAIN");
        let m1 = make_test_macro("M1");

        let mut mgr = ScopeManager::new();
        mgr.push_macro(&main, 0);
        mgr.push_macro(&m1, 5);

        assert_eq!(mgr.depth(), 2);

        mgr.pop_macro();
        assert_eq!(mgr.depth(), 1);
        assert_eq!(mgr.current_scope(), ScopeId::new(vec!["MAIN".to_string()], 0));

        mgr.pop_macro();
        assert_eq!(mgr.depth(), 0);
        assert_eq!(mgr.current_scope(), ScopeId::new(vec![], 0));
    }

    #[test]
    fn test_scope_id_parent() {
        let scope = ScopeId::new(vec!["MAIN".to_string(), "M1_5".to_string(), "M2_12".to_string()], 2);

        let parent = scope.parent().unwrap();
        assert_eq!(parent.path, vec!["MAIN".to_string(), "M1_5".to_string()]);
        assert_eq!(parent.depth, 1);

        let grandparent = parent.parent().unwrap();
        assert_eq!(grandparent.path, vec!["MAIN".to_string()]);
        assert_eq!(grandparent.depth, 0);

        assert!(grandparent.parent().is_none());
    }

    #[test]
    fn test_parent_scope() {
        let main = make_test_macro("MAIN");
        let m1 = make_test_macro("M1");

        let mut mgr = ScopeManager::new();
        mgr.push_macro(&main, 0);
        mgr.push_macro(&m1, 5);

        // parent_scope should return MAIN when in M1
        assert_eq!(mgr.parent_scope(), ScopeId::new(vec!["MAIN".to_string()], 0));
    }
}

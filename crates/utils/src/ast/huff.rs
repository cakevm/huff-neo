use crate::ast::abi::{Argument, EventDefinition, FunctionDefinition};
use crate::ast::span::AstSpan;
use crate::builtin_eval::{PadDirection, eval_builtin_bytes, eval_builtin_pad_simple, eval_event_hash, eval_function_signature};
use crate::{
    bytecode::*,
    bytes_util::*,
    error::{CodegenError, CodegenErrorKind},
    evm_version::EVMVersion,
    opcodes::{OPCODES_MAP, Opcode},
    prelude::{MacroArg::Ident, Span, TokenKind},
};
use alloy_primitives::U256;
use indexmap::IndexMap;
use std::collections::HashSet;
use std::{
    collections::BTreeMap,
    fmt::{Display, Formatter},
    path::PathBuf,
    sync::{Arc, Mutex},
};

/// A contained literal
pub type Literal = [u8; 32];

/// A File Path
///
/// Used for parsing the huff imports.
pub type FilePath = PathBuf;

/// A Huff Contract Representation
///
/// This is the representation of a contract as it is parsed from huff source code.
/// Thus, it is also the root of the AST.
///
/// For examples of Huff contracts, see the [huff-examples repository](https://github.com/huff-language/huff-examples).
#[derive(Debug, Default, Clone)]
pub struct Contract {
    // A IndexMap is used to preserve the insertion order of macros.
    // This helps to keep the resulting bytecode deterministic.
    /// Macro definitions, indexed by name
    pub macros: IndexMap<String, MacroDefinition>,
    /// Invocations of macros
    pub invocations: Vec<MacroInvocation>,
    /// File Imports
    pub imports: Vec<FilePath>,
    /// Constants
    pub constants: Arc<Mutex<Vec<ConstantDefinition>>>,
    /// Custom Errors
    pub errors: Vec<ErrorDefinition>,
    /// Functions
    pub functions: Vec<FunctionDefinition>,
    /// Events
    pub events: Vec<EventDefinition>,
    /// Tables
    pub tables: Vec<TableDefinition>,
    /// Labels
    pub labels: HashSet<String>,
    /// Flattened source code (for debugging and source maps)
    pub flattened_source: Option<String>,
    /// Individual source files for multi-file debugging support
    /// Vec of (file_path, source_content)
    pub source_files: Vec<(String, String)>,
    /// Mapping from flattened position to (file_index, original_position)
    pub source_map: Vec<(usize, usize, usize)>, // (flattened_start, file_id, original_start)
}

impl Contract {
    /// Map a position from the flattened source to the original file and position
    /// Returns (file_id, original_start, original_end)
    pub fn map_flattened_position_to_source(&self, flattened_start: usize, flattened_end: usize) -> (u32, usize, usize) {
        // Find which file this position belongs to
        let mut file_id = 0u32;
        let mut original_start = flattened_start;
        let mut original_end = flattened_end;

        // Iterate through source_map to find the right file
        for (idx, (file_start_in_flattened, fid, _)) in self.source_map.iter().enumerate() {
            // Check if we have a next entry to determine the file's end position
            let file_end = if idx + 1 < self.source_map.len() {
                self.source_map[idx + 1].0
            } else {
                // Last file extends to the end
                usize::MAX
            };

            // Check if this position is within this file's range
            if *file_start_in_flattened <= flattened_start && flattened_start < file_end {
                file_id = *fid as u32;
                // Map to position within this file (subtract the file's start position in flattened source)
                original_start = flattened_start - file_start_in_flattened;
                original_end = flattened_end - file_start_in_flattened;
                break;
            }
        }

        (file_id, original_start, original_end)
    }

    /// Convert a span from original file coordinates to flattened source coordinates
    /// This is needed because AST spans are in original file positions, but source maps need flattened positions
    pub fn map_original_span_to_flattened(&self, span: &crate::prelude::Span) -> Option<(usize, usize)> {
        if let Some(file) = &span.file {
            // Find this file in our source_files list
            let file_id = self.source_files.iter().enumerate().find(|(_, (path, _))| path == &file.path).map(|(id, _)| id)?;

            // Find the start position of this file in the flattened source
            let file_start_in_flattened = self.source_map.iter().find(|(_, fid, _)| *fid == file_id).map(|(start, _, _)| *start)?;

            // Convert original file positions to flattened positions
            let flattened_start = file_start_in_flattened + span.start;
            let flattened_end = file_start_in_flattened + span.end;

            Some((flattened_start, flattened_end))
        } else {
            None
        }
    }

    /// Returns the first macro that matches the provided name
    pub fn find_macro_by_name(&self, name: &str) -> Option<&MacroDefinition> {
        self.macros.get(name).or_else(|| {
            tracing::warn!("Failed to find macro \"{}\" in contract", name);
            None
        })
    }

    /// Returns the first table that matches the provided name
    pub fn find_table_by_name(&self, name: &str) -> Option<TableDefinition> {
        if let Some(t) = self.tables.iter().find(|t| t.name == name) {
            Some(t.clone())
        } else {
            tracing::warn!("Failed to find table \"{}\" in contract", name);
            None
        }
    }

    /// Derives the FreeStoragePointers into their bytes32 representation
    pub fn derive_storage_pointers(&mut self) {
        let mut storage_pointers: Vec<(String, [u8; 32])> = Vec::new();
        let mut last_assigned_free_pointer = 0;

        // Derive Constructor Storage Pointers
        match self.find_macro_by_name("CONSTRUCTOR") {
            Some(m) => self.recurse_ast_constants(m, &mut storage_pointers, &mut last_assigned_free_pointer, false),
            None => {
                // The constructor is not required, so we can just warn
                tracing::warn!(target: "ast", "'CONSTRUCTOR' MACRO NOT FOUND WHILE DERIVING STORAGE POINTERS!")
            }
        }

        // Derive Main Storage Pointers
        match self.find_macro_by_name("MAIN") {
            Some(m) => self.recurse_ast_constants(m, &mut storage_pointers, &mut last_assigned_free_pointer, false),
            None => {
                tracing::error!(target: "ast", "'MAIN' MACRO NOT FOUND WHILE DERIVING STORAGE POINTERS!")
            }
        }

        tracing::debug!(target: "ast", "Generate Storage pointers: {:?}", storage_pointers);
        tracing::debug!(target: "ast", "ALL AST CONSTANTS: {:?}", storage_pointers);

        // Set all the constants to their new values
        for c in self.constants.lock().unwrap().iter_mut() {
            match storage_pointers.iter().filter(|pointer| pointer.0.eq(&c.name)).collect::<Vec<&(String, [u8; 32])>>().first() {
                Some(p) => {
                    *c = ConstantDefinition { name: c.name.to_string(), value: ConstVal::StoragePointer(p.1), span: c.span.clone() };
                }
                None => {
                    tracing::warn!(target: "ast", "SET STORAGE POINTER BUT FAILED TO SET DERIVED CONSTANT VALUE FOR \"{}\"", c.name)
                }
            }
        }
    }

    /// Recurse down an AST Macro Definition to set Storage Pointers
    ///
    /// ## Overview
    ///
    /// For each statement in the macro definition:
    ///     - If it's a free storage pointer constant, set the constant value if not already set and
    ///       updated out `last_p` tracker value
    ///     - If it's a literal constant, we can set the constant value directly to the literal if
    ///       not already set
    ///     - If it's a macro invocation, look for the macro definition and recurse into that macro
    ///       definition using `recurse_ast_constants`
    pub fn recurse_ast_constants(
        &self,
        macro_def: &MacroDefinition,
        storage_pointers: &mut Vec<(String, [u8; 32])>,
        last_p: &mut i32,
        checking_constructor: bool,
    ) {
        let mut visited = std::collections::HashSet::new();
        self.recurse_ast_constants_inner(macro_def, storage_pointers, last_p, checking_constructor, &mut visited)
    }

    fn recurse_ast_constants_inner(
        &self,
        macro_def: &MacroDefinition,
        storage_pointers: &mut Vec<(String, [u8; 32])>,
        last_p: &mut i32,
        checking_constructor: bool,
        visited: &mut std::collections::HashSet<String>,
    ) {
        // Check for circular recursion
        if visited.contains(&macro_def.name) {
            tracing::warn!(target: "ast", "Circular macro invocation detected: '{}' is already being processed. Skipping to prevent infinite recursion.", macro_def.name);
            return;
        }

        // Mark this macro as being processed
        visited.insert(macro_def.name.clone());
        let mut statements = macro_def.statements.clone();

        let mut i = 0;
        loop {
            if i >= statements.len() {
                break;
            }
            match &statements[i].clone().ty {
                StatementType::Constant(const_name) => {
                    self.assign_free_storage_pointers(const_name, &macro_def.name, storage_pointers, last_p);
                }
                StatementType::MacroInvocation(mi) => {
                    tracing::debug!(target: "ast", "Found macro invocation: \"{}\" in macro def: \"{}\"!", mi.macro_name, macro_def.name);

                    // Check for constant references in macro arguments
                    let mut constant_args: Vec<String> = Vec::new();
                    for arg in &mi.args {
                        // check if it is a constant
                        if let Ident(name) = arg {
                            self.constants.lock().unwrap().iter().for_each(|constant| {
                                if name == &constant.name {
                                    tracing::debug!(target: "ast", "CONSTANT FOUND AS MACRO PARAMETER {}", name);
                                    constant_args.push(name.to_string());
                                }
                            })
                        }
                    }
                    // Assign constants that reference the Free Storage Pointer
                    for constant_arg in constant_args {
                        self.assign_free_storage_pointers(&constant_arg, &macro_def.name, storage_pointers, last_p);
                    }

                    match self.macros.get(&mi.macro_name) {
                        Some(md) => {
                            if md.name.eq("CONSTRUCTOR") {
                                if !checking_constructor {
                                    self.recurse_ast_constants_inner(md, storage_pointers, last_p, true, visited);
                                }
                            } else {
                                self.recurse_ast_constants_inner(md, storage_pointers, last_p, checking_constructor, visited);
                            }
                        }
                        None => {
                            tracing::warn!(target: "ast", "MACRO \"{}\" INVOKED BUT NOT FOUND IN AST!", mi.macro_name)
                        }
                    }
                }
                StatementType::BuiltinFunctionCall(bfc) => {
                    tracing::debug!(target: "ast", "Deriving Storage Pointers: Found builtin function {:?}", bfc.kind);
                    for builtin_fn_arg in &bfc.args {
                        let BuiltinFunctionArg::Argument(a) = builtin_fn_arg else { continue };
                        if let Some(name) = &a.name {
                            match self.macros.get(name) {
                                Some(md) => {
                                    if md.name.eq("CONSTRUCTOR") {
                                        if !checking_constructor {
                                            self.recurse_ast_constants_inner(md, storage_pointers, last_p, true, visited);
                                        }
                                    } else {
                                        self.recurse_ast_constants_inner(md, storage_pointers, last_p, checking_constructor, visited);
                                    }
                                }
                                None => {
                                    tracing::warn!(target: "ast", "BUILTIN HAS ARG NAME \"{}\" BUT NOT FOUND IN AST!", name)
                                }
                            }
                        }
                    }
                }
                StatementType::Label(l) => {
                    for state in l.inner.iter().rev() {
                        statements.insert(i + 1, state.clone());
                    }
                }
                _ => {}
            }
            i += 1;
        }

        // Remove this macro from visited set when done processing it
        visited.remove(&macro_def.name);

        // Breadth-first
        // if !macros_to_recurse.is_empty() {
        //     let next_md = macros_to_recurse.remove(0);
        //     self.recurse_ast_constants(next_md, storage_pointers, last_p, macros_to_recurse);
        // }
    }

    fn assign_free_storage_pointers(
        &self,
        const_name: &String,
        macro_name: &String,
        storage_pointers: &mut Vec<(String, [u8; 32])>,
        last_p: &mut i32,
    ) {
        tracing::debug!(target: "ast", "Found constant \"{}\" in macro def \"{}\" statements!", const_name, macro_name);
        if storage_pointers.iter().filter(|pointer| pointer.0.eq(const_name)).collect::<Vec<&(String, [u8; 32])>>().is_empty() {
            tracing::debug!(target: "ast", "No storage pointer already set for \"{}\"!", const_name);
            // Get the associated constant
            match self.constants.lock().unwrap().iter().filter(|c| c.name.eq(const_name)).collect::<Vec<&ConstantDefinition>>().first() {
                Some(c) => {
                    match c.value {
                        ConstVal::FreeStoragePointer(_) => {
                            let old_p = *last_p;
                            *last_p += 1;
                            let new_value = str_to_bytes32(&format!("{old_p}"));
                            storage_pointers.push((const_name.to_string(), new_value));
                        }
                        ConstVal::Bytes(_) | ConstVal::BuiltinFunctionCall(_) | ConstVal::Expression(_) | ConstVal::Noop => {
                            // Skip constants that are not free storage pointers
                        }
                        // This should never be reached, as we only assign free storage pointers
                        _ => panic!("Invalid Constant Value"),
                    };
                }
                None => {
                    tracing::warn!(target: "ast", "CONSTANT \"{}\" NOT FOUND IN AST CONSTANTS", const_name)
                }
            }
        }
    }

    /// Add override constants to the AST
    ///
    /// ## Overview
    ///
    /// For each override constant, add it to the AST if it doesn't already exist. Override
    /// constants can be passed in via the CLI.
    pub fn add_override_constants(&self, override_constants: &Option<BTreeMap<&str, Bytes>>) {
        if let Some(override_constants) = override_constants {
            for (name, value) in override_constants {
                let mut constants = self.constants.lock().unwrap();
                if let Some(c) = constants.iter_mut().find(|c| c.name.as_str().eq(*name)) {
                    c.value = ConstVal::Bytes(value.clone());
                } else {
                    constants.push(ConstantDefinition {
                        name: name.to_string(),
                        value: ConstVal::Bytes(value.clone()),
                        span: AstSpan::default(),
                    });
                }
            }
        }
    }

    /// Validate that all opcodes used in the contract are compatible with the target EVM version
    pub fn validate_opcodes(&self, evm_version: &EVMVersion) -> Result<(), CodegenError> {
        for macro_def in self.macros.values() {
            Self::validate_statements_opcodes(&macro_def.statements, evm_version)?;
        }

        for table in &self.tables {
            Self::validate_statements_opcodes(&table.statements, evm_version)?;
        }

        Ok(())
    }

    /// Helper function to recursively validate opcodes in a list of statements
    fn validate_statements_opcodes(statements: &[Statement], evm_version: &EVMVersion) -> Result<(), CodegenError> {
        for statement in statements {
            match &statement.ty {
                StatementType::Opcode(opcode) => {
                    if let Some(required_version) = opcode.requires_evm_version()
                        && evm_version.version() < &required_version
                    {
                        return Err(CodegenError {
                            kind: CodegenErrorKind::InvalidOpcodeForEVMVersion(
                                opcode.as_ref().to_string(),
                                required_version.to_string(),
                                evm_version.to_string(),
                            ),
                            span: statement.span.clone_box(),
                            token: Some(TokenKind::Opcode(*opcode)),
                        });
                    }
                }
                StatementType::Label(label) => {
                    // Recursively validate statements inside labels
                    Self::validate_statements_opcodes(&label.inner, evm_version)?;
                }
                StatementType::MacroInvocation(invocation) => {
                    // Validate opcodes passed as arguments to the macro
                    Self::validate_macro_invocation_args(&invocation.args, evm_version, &statement.span)?;
                }
                StatementType::ArgMacroInvocation(_, _, args) => {
                    // Validate opcodes passed as arguments to the arg macro invocation
                    Self::validate_macro_invocation_args(args, evm_version, &statement.span)?;
                }
                _ => {
                    // Other statement types don't contain opcodes we need to validate
                }
            }
        }
        Ok(())
    }

    /// Helper function to validate opcodes passed as macro invocation arguments
    fn validate_macro_invocation_args(args: &[MacroArg], evm_version: &EVMVersion, invocation_span: &AstSpan) -> Result<(), CodegenError> {
        for arg in args {
            match arg {
                MacroArg::Ident(name) => {
                    // Check if this identifier is an opcode name
                    if let Some(&opcode) = OPCODES_MAP.get(name.as_str()) {
                        // Validate the opcode against the EVM version
                        if let Some(required_version) = opcode.requires_evm_version()
                            && evm_version.version() < &required_version
                        {
                            return Err(CodegenError {
                                kind: CodegenErrorKind::InvalidOpcodeForEVMVersion(
                                    opcode.as_ref().to_string(),
                                    required_version.to_string(),
                                    evm_version.to_string(),
                                ),
                                span: invocation_span.clone_box(),
                                token: Some(TokenKind::Opcode(opcode)),
                            });
                        }
                    }
                }
                MacroArg::MacroCall(invocation) => {
                    // Recursively validate nested macro invocations
                    Self::validate_macro_invocation_args(&invocation.args, evm_version, &invocation.span)?;
                }
                MacroArg::ArgCallMacroInvocation(_, nested_args) => {
                    // Recursively validate arg call macro invocations
                    // Note: We use the parent invocation span since arg calls don't have their own span
                    Self::validate_macro_invocation_args(nested_args, evm_version, invocation_span)?;
                }
                _ => {
                    // Literals and ArgCalls don't need validation
                }
            }
        }
        Ok(())
    }

    /// Recursively evaluates arithmetic expressions in constant definitions at compile time.
    /// This supports binary operations (+, -, *, /, %), unary negation, literals, constant references,
    /// and parenthesized expressions with proper operator precedence and overflow/underflow checking.
    pub fn evaluate_constant_expression(&self, expr: &Expression) -> Result<Literal, CodegenError> {
        // Start evaluation with an empty set of currently-evaluating constants
        let mut evaluating = std::collections::HashSet::new();
        self.evaluate_constant_expression_internal(expr, &mut evaluating)
    }

    /// Internal helper that tracks which constants are currently being evaluated to detect cycles
    fn evaluate_constant_expression_internal(
        &self,
        expr: &Expression,
        evaluating: &mut std::collections::HashSet<String>,
    ) -> Result<Literal, CodegenError> {
        match expr {
            Expression::Literal { value, .. } => {
                // Direct literal value
                Ok(*value)
            }

            Expression::Constant { name, span } => {
                // Check for circular dependency
                if evaluating.contains(name) {
                    return Err(CodegenError {
                        kind: CodegenErrorKind::CircularConstantDependency(name.clone()),
                        span: span.clone_box(),
                        token: None,
                    });
                }

                // Mark this constant as being evaluated
                evaluating.insert(name.clone());

                // Look up the constant by name and clone the value
                // We must clone before recursing to avoid holding the lock during recursion
                let const_value = {
                    let constants = self.constants.lock().unwrap();
                    let const_def = constants.iter().find(|c| c.name == *name).ok_or_else(|| CodegenError {
                        kind: CodegenErrorKind::UndefinedConstant(name.clone()),
                        span: span.clone_box(),
                        token: None,
                    })?;
                    const_def.value.clone()
                };

                // Evaluate the constant's value (lock is released at this point)
                let result = match &const_value {
                    ConstVal::Bytes(bytes) => {
                        // Parse hex string to [u8; 32]
                        Ok(str_to_bytes32(&bytes.as_str()))
                    }
                    ConstVal::Expression(e) => {
                        // Recursively evaluate nested expressions
                        self.evaluate_constant_expression_internal(e, evaluating)
                    }
                    ConstVal::StoragePointer(lit) => {
                        // Storage pointers are already resolved literals
                        Ok(*lit)
                    }
                    ConstVal::BuiltinFunctionCall(bf) => {
                        // Evaluate builtin functions at compile time
                        let push_value = match bf.kind {
                            BuiltinFunctionKind::FunctionSignature => eval_function_signature(self, bf)?,
                            BuiltinFunctionKind::EventHash => eval_event_hash(self, bf)?,
                            BuiltinFunctionKind::Bytes => eval_builtin_bytes(bf)?,
                            BuiltinFunctionKind::RightPad => eval_builtin_pad_simple(bf, PadDirection::Right)?,
                            BuiltinFunctionKind::LeftPad => eval_builtin_pad_simple(bf, PadDirection::Left)?,
                            _ => {
                                return Err(CodegenError {
                                    kind: CodegenErrorKind::UnsupportedBuiltinFunction(format!("{}", bf.kind)),
                                    span: span.clone_box(),
                                    token: None,
                                });
                            }
                        };
                        // Extract the [u8; 32] value from PushValue
                        Ok(push_value.value.0)
                    }
                    _ => Err(CodegenError { kind: CodegenErrorKind::InvalidConstantExpression, span: span.clone_box(), token: None }),
                };

                // Remove from evaluating set after evaluation completes
                evaluating.remove(name);

                result
            }

            Expression::Binary { left, op, right, span } => {
                // Evaluate both operands
                let left_val = self.evaluate_constant_expression_internal(left, evaluating)?;
                let right_val = self.evaluate_constant_expression_internal(right, evaluating)?;

                // Convert to U256 for arithmetic
                let l = U256::from_be_bytes(left_val);
                let r = U256::from_be_bytes(right_val);

                // Perform the operation with overflow checking
                let result = match op {
                    BinaryOp::Add => l.checked_add(r).ok_or_else(|| CodegenError {
                        kind: CodegenErrorKind::ArithmeticOverflow,
                        span: span.clone_box(),
                        token: None,
                    })?,
                    BinaryOp::Sub => l.checked_sub(r).ok_or_else(|| CodegenError {
                        kind: CodegenErrorKind::ArithmeticUnderflow,
                        span: span.clone_box(),
                        token: None,
                    })?,
                    BinaryOp::Mul => l.checked_mul(r).ok_or_else(|| CodegenError {
                        kind: CodegenErrorKind::ArithmeticOverflow,
                        span: span.clone_box(),
                        token: None,
                    })?,
                    BinaryOp::Div => {
                        if r.is_zero() {
                            return Err(CodegenError { kind: CodegenErrorKind::DivisionByZero, span: span.clone_box(), token: None });
                        }
                        l / r
                    }
                    BinaryOp::Mod => {
                        if r.is_zero() {
                            return Err(CodegenError { kind: CodegenErrorKind::DivisionByZero, span: span.clone_box(), token: None });
                        }
                        l % r
                    }
                    // Comparison operators return 1 (true) or 0 (false)
                    BinaryOp::Eq => {
                        if l == r {
                            U256::from(1u64)
                        } else {
                            U256::ZERO
                        }
                    }
                    BinaryOp::NotEq => {
                        if l != r {
                            U256::from(1u64)
                        } else {
                            U256::ZERO
                        }
                    }
                    BinaryOp::Lt => {
                        if l < r {
                            U256::from(1u64)
                        } else {
                            U256::ZERO
                        }
                    }
                    BinaryOp::Gt => {
                        if l > r {
                            U256::from(1u64)
                        } else {
                            U256::ZERO
                        }
                    }
                    BinaryOp::LtEq => {
                        if l <= r {
                            U256::from(1u64)
                        } else {
                            U256::ZERO
                        }
                    }
                    BinaryOp::GtEq => {
                        if l >= r {
                            U256::from(1u64)
                        } else {
                            U256::ZERO
                        }
                    }
                };

                Ok(result.to_be_bytes())
            }

            Expression::Unary { op, expr, .. } => {
                // Evaluate the operand
                let val = self.evaluate_constant_expression_internal(expr, evaluating)?;
                let v = U256::from_be_bytes(val);

                // Perform unary operation
                let result = match op {
                    UnaryOp::Neg => {
                        // Two's complement negation
                        U256::ZERO.wrapping_sub(v)
                    }
                    UnaryOp::Not => {
                        // Logical NOT: 0 becomes 1, non-zero becomes 0
                        if v.is_zero() { U256::from(1u64) } else { U256::ZERO }
                    }
                };

                Ok(result.to_be_bytes())
            }

            Expression::Grouped { expr, .. } => {
                // Grouped expressions just evaluate the inner expression
                self.evaluate_constant_expression_internal(expr, evaluating)
            }

            Expression::ArgCall { name, span, .. } => {
                // ArgCall cannot be evaluated without macro invocation context
                // This should only be reached during global expansion (before macro expansion)
                // When if/for expansion moves to statement_gen, this won't be called
                Err(CodegenError { kind: CodegenErrorKind::ArgCallInConstantExpression(name.clone()), span: span.clone_box(), token: None })
            }
        }
    }
}

/// A Table Definition
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TableDefinition {
    /// The name of the table
    pub name: String,
    /// The table kind
    pub kind: TableKind,
    /// The table's statements
    pub statements: Vec<Statement>,
    /// Size of table if known. If built-in function is used, this will be `None` and the size will be calculated during codegen.
    pub size: Option<Literal>,
    /// The table span
    pub span: AstSpan,
}

impl TableDefinition {
    /// Public associated function that instantiates a TableDefinition from a string
    pub fn new(name: String, kind: TableKind, statements: Vec<Statement>, size: Option<Literal>, span: AstSpan) -> Self {
        TableDefinition { name, kind, statements, size, span }
    }
}

/// A Table Kind
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TableKind {
    /// A regular jump table
    JumpTable,
    /// A packed jump table
    JumpTablePacked,
    /// A code table
    CodeTable,
}

impl From<TokenKind> for TableKind {
    /// Public associated function that converts a TokenKind to a TableKind
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::JumpTable => TableKind::JumpTable,
            TokenKind::JumpTablePacked => TableKind::JumpTablePacked,
            TokenKind::CodeTable => TableKind::CodeTable,
            _ => panic!("Invalid Token Kind"), // TODO: Better error handling
        }
    }
}

/// A Macro Definition
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MacroDefinition {
    /// The Macro Name
    pub name: String,
    /// The macro's decorator
    pub decorator: Option<Decorator>,
    /// A list of Macro parameters
    pub parameters: Vec<Argument>,
    /// A list of Statements contained in the Macro
    pub statements: Vec<Statement>,
    /// The take size
    pub takes: usize,
    /// The return size
    pub returns: usize,
    /// The Span of the Macro Definition
    pub span: AstSpan,
    /// Is the macro a function (outlined)?
    pub outlined: bool,
    /// Is the macro a test?
    pub test: bool,
}

impl ToIRBytecode<CodegenError> for MacroDefinition {
    fn to_irbytecode(&self, evm_version: &EVMVersion) -> Result<IRBytecode<'_>, CodegenError> {
        let inner_irbytes: Vec<IRBytes> = MacroDefinition::to_irbytes(evm_version, &self.statements);
        Ok(IRBytecode(inner_irbytes))
    }
}

impl MacroDefinition {
    /// Public associated function that instantiates a MacroDefinition.
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        name: String,
        decorator: Option<Decorator>,
        parameters: Vec<Argument>,
        statements: Vec<Statement>,
        takes: usize,
        returns: usize,
        spans: Vec<Span>,
        outlined: bool,
        test: bool,
    ) -> Self {
        MacroDefinition { name, decorator, parameters, statements, takes, returns, span: AstSpan(spans), outlined, test }
    }

    /// Translate statements into IRBytes
    pub fn to_irbytes<'a>(evm_version: &EVMVersion, statements: &'a [Statement]) -> Vec<IRBytes<'a>> {
        let mut inner_irbytes: Vec<IRBytes> = vec![];

        let mut statement_iter = statements.iter();
        while let Some(statement) = statement_iter.next() {
            match &statement.ty {
                StatementType::Literal(l) => {
                    let push_bytes = literal_gen(evm_version, l);
                    inner_irbytes.push(IRBytes { ty: IRByteType::Bytes(Bytes::Raw(push_bytes)), span: &statement.span });
                }
                StatementType::Opcode(o) => {
                    let opcode_str = o.string();
                    inner_irbytes.push(IRBytes { ty: IRByteType::Bytes(Bytes::Raw(opcode_str)), span: &statement.span });
                    // If the opcode is a push that takes a literal value, we need to consume the
                    // next statement, which must be a literal as checked in the parser
                    if o.is_value_push() {
                        match statement_iter.next() {
                            Some(Statement { ty: StatementType::Literal(l), span: _ }) => {
                                let hex_literal: String = bytes32_to_hex_string(l, false);
                                let prefixed_hex_literal = o.prefix_push_literal(&hex_literal);
                                inner_irbytes
                                    .push(IRBytes { ty: IRByteType::Bytes(Bytes::Raw(prefixed_hex_literal)), span: &statement.span });
                            }
                            _ => {
                                // We have a push without a literal - this should be caught by the
                                // parser
                                panic!("Invalid push statement");
                            }
                        }
                    }
                }
                StatementType::Code(c) => {
                    inner_irbytes.push(IRBytes { ty: IRByteType::Bytes(Bytes::Raw(c.to_owned())), span: &statement.span });
                }
                StatementType::MacroInvocation(mi) => {
                    inner_irbytes.push(IRBytes {
                        ty: IRByteType::Statement(Box::new(Statement {
                            ty: StatementType::MacroInvocation(mi.clone()),
                            span: statement.span.clone(),
                        })),
                        span: &statement.span,
                    });
                }
                StatementType::Constant(name) => {
                    // Constant needs to be evaluated at the top-level
                    inner_irbytes.push(IRBytes { ty: IRByteType::Constant(name.to_owned()), span: &statement.span });
                }
                StatementType::ArgCall(parent_macro_name, arg_name) => {
                    // Arg call needs to use a destination defined in the calling macro context
                    inner_irbytes.push(IRBytes {
                        ty: IRByteType::ArgCall(parent_macro_name.to_owned(), arg_name.to_owned()),
                        span: &statement.span,
                    });
                }
                StatementType::ArgMacroInvocation(parent_macro_name, arg_name, args) => {
                    // Arg macro invocation - will be resolved to actual macro invocation during codegen
                    inner_irbytes.push(IRBytes {
                        ty: IRByteType::Statement(Box::new(Statement {
                            ty: StatementType::ArgMacroInvocation(parent_macro_name.clone(), arg_name.clone(), args.clone()),
                            span: statement.span.clone(),
                        })),
                        span: &statement.span,
                    });
                }
                StatementType::LabelCall(jump_to) => {
                    /* Jump To doesn't translate directly to bytecode */
                    inner_irbytes.push(IRBytes {
                        ty: IRByteType::Statement(Box::new(Statement {
                            ty: StatementType::LabelCall(jump_to.to_string()),
                            span: statement.span.clone(),
                        })),
                        span: &statement.span,
                    });
                }
                StatementType::Label(l) => {
                    /* Jump Dests don't translate directly to bytecode */
                    inner_irbytes.push(IRBytes {
                        ty: IRByteType::Statement(Box::new(Statement {
                            ty: StatementType::Label(l.clone()),
                            span: statement.span.clone(),
                        })),
                        span: &statement.span,
                    });

                    // Recurse label statements to IRBytes Bytes
                    inner_irbytes.append(&mut MacroDefinition::to_irbytes(evm_version, &l.inner));
                }
                StatementType::BuiltinFunctionCall(builtin) => {
                    inner_irbytes.push(IRBytes {
                        ty: IRByteType::Statement(Box::new(Statement {
                            ty: StatementType::BuiltinFunctionCall(builtin.clone()),
                            span: statement.span.clone(),
                        })),
                        span: &statement.span,
                    });
                }
                StatementType::ForLoop { .. } => {
                    // ForLoop will be expanded during statement_gen with macro invocation context
                    inner_irbytes.push(IRBytes { ty: IRByteType::Statement(Box::new(statement.clone())), span: &statement.span });
                }
                StatementType::IfStatement { .. } => {
                    // IfStatement will be expanded during statement_gen with macro invocation context
                    inner_irbytes.push(IRBytes { ty: IRByteType::Statement(Box::new(statement.clone())), span: &statement.span });
                }
            }
        }

        inner_irbytes
    }
}

/// A Macro Invocation
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MacroInvocation {
    /// The Macro Name
    pub macro_name: String,
    /// A list of Macro arguments
    pub args: Vec<MacroArg>,
    /// The Macro Invocation Span
    pub span: AstSpan,
}

/// An argument passed when invoking a macro
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MacroArg {
    /// Macro Literal Argument
    ///
    /// Example: `MACRO(0x04)`, `MACRO(0x420)`
    Literal(Literal),
    /// Macro Iden String Argument
    ///
    /// Example: `MACRO(my_label)`, `MACRO(SOME_CONSTANT)`
    Ident(String),
    /// An Arg Call
    ///
    /// Passing through an argument from outer macro. Example: `INNER(<value>)`
    ArgCall(ArgCall),
    /// An Arg Call that is invoked as a macro
    ///
    /// First-class macro invocation. Example: `APPLY(<operation>())`
    ArgCallMacroInvocation(String, Vec<MacroArg>),
    /// A Nested Macro Call
    ///
    /// Example: `OUTER(INNER())`, `TRANSFER(CALCULATE_AMOUNT())`
    MacroCall(MacroInvocation),
    /// Opcode Argument
    ///
    /// Example: `APPLY_OP(add)`, `USE_TWO_OPS(tload, clz)`
    Opcode(Opcode),
    /// Builtin Function Call Argument
    ///
    /// Example: `MACRO(__FUNC_SIG(transfer))`, `MACRO(__EVENT_HASH(Transfer))`
    BuiltinFunctionCall(BuiltinFunctionCall),
    /// __NOOP builtin constant (generates no bytecode)
    ///
    /// Example: `MACRO(__NOOP)`
    Noop,
}

/// An argument call
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArgCall {
    /// The Macro Name
    pub macro_name: String,
    /// The name of the argument
    pub name: String,
    /// The span of the argument call
    pub span: AstSpan,
}

/// Free Storage Pointer Unit Struct
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FreeStoragePointer;

/// An arithmetic expression node
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    /// A literal value
    Literal {
        /// The literal value
        value: Literal,
        /// The span of the literal
        span: AstSpan,
    },
    /// Reference to a named constant
    Constant {
        /// The name of the constant
        name: String,
        /// The span of the constant reference
        span: AstSpan,
    },
    /// Binary operation (e.g., 1 + 2, 3 * 4)
    Binary {
        /// Left operand
        left: Box<Expression>,
        /// Binary operator
        op: BinaryOp,
        /// Right operand
        right: Box<Expression>,
        /// The span of the binary operation
        span: AstSpan,
    },
    /// Unary operation (e.g., -5)
    Unary {
        /// Unary operator
        op: UnaryOp,
        /// Operand expression
        expr: Box<Expression>,
        /// The span of the unary operation
        span: AstSpan,
    },
    /// Grouped/parenthesized expression
    Grouped {
        /// The inner expression
        expr: Box<Expression>,
        /// The span of the grouped expression (including parentheses)
        span: AstSpan,
    },
    /// Reference to a macro argument (e.g., `<arg>`)
    ArgCall {
        /// The parent macro name
        macro_name: String,
        /// The argument name
        name: String,
        /// The span of the arg call
        span: AstSpan,
    },
}

impl Expression {
    /// Get the span of this expression
    pub fn span(&self) -> &AstSpan {
        match self {
            Expression::Literal { span, .. } => span,
            Expression::Constant { span, .. } => span,
            Expression::Binary { span, .. } => span,
            Expression::Unary { span, .. } => span,
            Expression::Grouped { span, .. } => span,
            Expression::ArgCall { span, .. } => span,
        }
    }
}

/// Binary operators for arithmetic and comparison expressions
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOp {
    /// Addition (+)
    Add,
    /// Subtraction (-)
    Sub,
    /// Multiplication (*)
    Mul,
    /// Division (/)
    Div,
    /// Modulo (%)
    Mod,
    /// Equal comparison (==)
    Eq,
    /// Not equal comparison (!=)
    NotEq,
    /// Less than (<)
    Lt,
    /// Greater than (>)
    Gt,
    /// Less than or equal (<=)
    LtEq,
    /// Greater than or equal (>=)
    GtEq,
}

/// Unary operators for arithmetic and logical expressions
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOp {
    /// Negation (-)
    Neg,
    /// Logical NOT (!)
    Not,
}

/// A Constant Value
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConstVal {
    /// Bytes value for the constant
    Bytes(Bytes),
    /// A Free Storage Pointer
    FreeStoragePointer(FreeStoragePointer),
    /// A Storage Pointer assigned by the compiler
    StoragePointer(Literal),
    /// Built-in function call
    BuiltinFunctionCall(BuiltinFunctionCall),
    /// An arithmetic expression (evaluated at compile time)
    Expression(Expression),
    /// __NOOP builtin constant (generates no bytecode)
    Noop,
}

/// A Constant Definition
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstantDefinition {
    /// The Constant name
    pub name: String,
    /// The Constant value
    pub value: ConstVal,
    /// The Span of the Constant Definition
    pub span: AstSpan,
}

/// An Error Definition
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ErrorDefinition {
    /// The Error name
    pub name: String,
    /// The Error's selector
    pub selector: [u8; 4],
    /// The parameters of the error
    pub parameters: Vec<Argument>,
    /// The Span of the Constant Definition
    pub span: AstSpan,
}

/// A Jump Destination
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Label {
    /// The JumpDest Name
    pub name: String,
    /// Statements Inside The JumpDest
    pub inner: Vec<Statement>,
    /// The label span
    pub span: AstSpan,
}

/// A Builtin Function Argument
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinFunctionArg {
    /// A Literal Argument
    Literal(Literal),
    /// Builtin Function Call
    BuiltinFunctionCall(BuiltinFunctionCall),
    // TODO: Remove this and replace with a better type
    /// Abi Argument
    Argument(Argument),
    /// Constant Argument
    Constant(String, AstSpan),
}

impl BuiltinFunctionArg {
    /// Get the span of the Builtin Function Argument
    pub fn span(&self) -> AstSpan {
        match self {
            BuiltinFunctionArg::Literal(_) => AstSpan::default(),
            BuiltinFunctionArg::BuiltinFunctionCall(b) => b.span.clone(),
            BuiltinFunctionArg::Argument(a) => a.span.clone(),
            BuiltinFunctionArg::Constant(_, span) => span.clone(),
        }
    }
}

/// A Builtin Function Call
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BuiltinFunctionCall {
    /// The Builtin Kind
    pub kind: BuiltinFunctionKind,
    /// Arguments for the builtin function call.
    /// TODO: Maybe make a better type for this other than `Argument`? Would be nice if it pointed
    ///       directly to the macro/table.
    /// Update: Spitted with BuiltinFunctionArg, but still needs to be update for macro/table
    pub args: Vec<BuiltinFunctionArg>,
    /// The builtin function call span
    pub span: AstSpan,
}

/// A Builtin Function Kind
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinFunctionKind {
    /// Table size function
    Tablesize,
    /// Code size function
    Codesize,
    /// Table start function
    Tablestart,
    /// Function signature function
    FunctionSignature,
    /// Event hash function
    EventHash,
    /// Error selector function
    Error,
    /// Rightpad function
    RightPad,
    /// Leftpad function (only available in code tables)
    LeftPad,
    /// Dynamic constructor arg function
    DynConstructorArg,
    /// Inject Raw Bytes
    Verbatim,
    /// Bytes function to convert a string to bytes
    Bytes,
    /// Assert PC (program counter) position
    AssertPc,
}

impl From<String> for BuiltinFunctionKind {
    fn from(value: String) -> Self {
        match value.as_str() {
            "__tablesize" => BuiltinFunctionKind::Tablesize,
            "__codesize" => BuiltinFunctionKind::Codesize,
            "__tablestart" => BuiltinFunctionKind::Tablestart,
            "__FUNC_SIG" => BuiltinFunctionKind::FunctionSignature,
            "__EVENT_HASH" => BuiltinFunctionKind::EventHash,
            "__ERROR" => BuiltinFunctionKind::Error,
            "__RIGHTPAD" => BuiltinFunctionKind::RightPad,
            "__LEFTPAD" => BuiltinFunctionKind::LeftPad,
            "__CODECOPY_DYN_ARG" => BuiltinFunctionKind::DynConstructorArg,
            "__VERBATIM" => BuiltinFunctionKind::Verbatim,
            "__BYTES" => BuiltinFunctionKind::Bytes,
            "__ASSERT_PC" => BuiltinFunctionKind::AssertPc,
            _ => panic!("Invalid Builtin Function Kind"), /* This should never be reached,
                                                           * builtins are validated with a
                                                           * `try_from` call in the lexer. */
        }
    }
}

impl TryFrom<&String> for BuiltinFunctionKind {
    type Error = ();

    fn try_from(value: &String) -> Result<Self, <BuiltinFunctionKind as TryFrom<&String>>::Error> {
        match value.as_str() {
            "__tablesize" => Ok(BuiltinFunctionKind::Tablesize),
            "__codesize" => Ok(BuiltinFunctionKind::Codesize),
            "__tablestart" => Ok(BuiltinFunctionKind::Tablestart),
            "__FUNC_SIG" => Ok(BuiltinFunctionKind::FunctionSignature),
            "__EVENT_HASH" => Ok(BuiltinFunctionKind::EventHash),
            "__ERROR" => Ok(BuiltinFunctionKind::Error),
            "__RIGHTPAD" => Ok(BuiltinFunctionKind::RightPad),
            "__LEFTPAD" => Ok(BuiltinFunctionKind::LeftPad),
            "__CODECOPY_DYN_ARG" => Ok(BuiltinFunctionKind::DynConstructorArg),
            "__VERBATIM" => Ok(BuiltinFunctionKind::Verbatim),
            "__BYTES" => Ok(BuiltinFunctionKind::Bytes),
            "__ASSERT_PC" => Ok(BuiltinFunctionKind::AssertPc),
            _ => Err(()),
        }
    }
}

impl Display for BuiltinFunctionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltinFunctionKind::Tablesize => write!(f, "__tablesize"),
            BuiltinFunctionKind::Codesize => write!(f, "__codesize"),
            BuiltinFunctionKind::Tablestart => write!(f, "__tablestart"),
            BuiltinFunctionKind::FunctionSignature => write!(f, "__FUNC_SIG"),
            BuiltinFunctionKind::EventHash => write!(f, "__EVENT_HASH"),
            BuiltinFunctionKind::Error => write!(f, "__ERROR"),
            BuiltinFunctionKind::RightPad => write!(f, "__RIGHTPAD"),
            BuiltinFunctionKind::LeftPad => write!(f, "__LEFTPAD"),
            BuiltinFunctionKind::DynConstructorArg => write!(f, "__CODECOPY_DYN_ARG"),
            BuiltinFunctionKind::Verbatim => write!(f, "__VERBATIM"),
            BuiltinFunctionKind::Bytes => write!(f, "__BYTES"),
            BuiltinFunctionKind::AssertPc => write!(f, "__ASSERT_PC"),
        }
    }
}

impl BuiltinFunctionKind {
    /// Checks if a string matches any reserved builtin function name
    pub fn is_reserved_name(name: &str) -> bool {
        matches!(
            name,
            "__tablesize"
                | "__codesize"
                | "__tablestart"
                | "__FUNC_SIG"
                | "__EVENT_HASH"
                | "__ERROR"
                | "__RIGHTPAD"
                | "__LEFTPAD"
                | "__CODECOPY_DYN_ARG"
                | "__VERBATIM"
                | "__BYTES"
                | "__ASSERT_PC"
                | "__NOOP"
        )
    }
}

/// A Statement
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Statement {
    /// The type of statement
    pub ty: StatementType,
    /// The span of the Statement
    pub span: AstSpan,
}

/// The Statement Type
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum StatementType {
    /// A Literal Statement
    ///
    /// Example: `0x04`, `0x420`
    Literal(Literal),
    /// An Opcode Statement
    ///
    /// Example: `add`, `mstore`, `calldataload`
    Opcode(Opcode),
    /// A Code Statement
    ///
    /// Raw bytecode string
    Code(String),
    /// A Macro Invocation Statement
    ///
    /// Example: `TRANSFER()`, `ONLY_OWNER()`
    MacroInvocation(MacroInvocation),
    /// A Constant Push
    ///
    /// Example: `[BALANCE_LOCATION]`, `[MAX_SUPPLY]`
    Constant(String),
    /// An Arg Call
    ///
    /// Referencing a macro parameter. Example: `<amount>`, `<recipient>`
    ///
    /// Macro name and argument name
    ArgCall(String, String),
    /// A Macro Invocation through Argument
    ///
    /// Calling a macro passed as an argument. Example: `<operation>()`
    ///
    /// Parent macro name, argument name, and arguments for the invoked macro
    ArgMacroInvocation(String, String, Vec<MacroArg>),
    /// A Label
    ///
    /// Jump destination definition. Example: `success:`, `error:`
    Label(Label),
    /// A Label Reference/Call
    ///
    /// Reference to jump to a label. Example: `success`, `error`
    LabelCall(String),
    /// A built-in function call
    ///
    /// Example: `__FUNC_SIG(transfer)`, `__EVENT_HASH(Transfer)`
    BuiltinFunctionCall(BuiltinFunctionCall),
    /// A compile-time for loop
    ///
    /// Example: `for(i in 0..5) { <i> }`
    ForLoop {
        /// Loop variable name
        variable: String,
        /// Start value (constant expression)
        start: Expression,
        /// End value (exclusive, constant expression)
        end: Expression,
        /// Optional step value (defaults to 1)
        step: Option<Expression>,
        /// Loop body statements
        body: Vec<Statement>,
    },
    /// A compile-time if statement
    ///
    /// Example: `if([CONST] > 5) { ... } else { ... }`
    IfStatement {
        /// Condition (constant expression that evaluates to non-zero = true)
        condition: Expression,
        /// Then branch statements
        then_branch: Vec<Statement>,
        /// Optional else if clauses (condition, body pairs)
        else_if_branches: Vec<(Expression, Vec<Statement>)>,
        /// Optional else branch statements
        else_branch: Option<Vec<Statement>>,
    },
}

impl Display for StatementType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementType::Literal(l) => write!(f, "LITERAL: {}", bytes32_to_hex_string(l, true)),
            StatementType::Opcode(o) => write!(f, "OPCODE: {o}"),
            StatementType::Code(s) => write!(f, "CODE: {s}"),
            StatementType::MacroInvocation(m) => {
                write!(f, "MACRO INVOCATION: {}", m.macro_name)
            }
            StatementType::Constant(c) => write!(f, "CONSTANT: {c}"),
            StatementType::ArgCall(m, c) => write!(f, "ARG CALL in {m}: {c}"),
            StatementType::ArgMacroInvocation(parent_macro, arg_name, args) => {
                write!(f, "ARG MACRO INVOCATION: <{}>({:?}) in {}", arg_name, args, parent_macro)
            }
            StatementType::Label(l) => write!(f, "LABEL: {}", l.name),
            StatementType::LabelCall(l) => write!(f, "LABEL CALL: {l}"),
            StatementType::BuiltinFunctionCall(b) => {
                write!(f, "BUILTIN FUNCTION CALL: {:?}", b.kind)
            }
            StatementType::ForLoop { variable, start: _, end: _, step, body } => {
                write!(f, "FOR LOOP: {} in ... step {:?}, {} statements", variable, step, body.len())
            }
            StatementType::IfStatement { condition: _, then_branch, else_if_branches, else_branch } => {
                write!(
                    f,
                    "IF STATEMENT: then {} statements, {} else if branches, else branch: {}",
                    then_branch.len(),
                    else_if_branches.len(),
                    else_branch.is_some()
                )
            }
        }
    }
}

/// A decorator tag
///
/// At the moment, the decorator tag can only be placed over test definitions. Developers
/// can use decorators to define environment variables and other metadata for their individual
/// tests.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Decorator {
    /// Vector of flags passed within the decorator
    pub flags: Vec<DecoratorFlag>,
}

/// A decorator flag
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DecoratorFlag {
    /// Sets the calldata of the test call transaction
    Calldata(String),
    /// Sets the value of the test call transaction
    Value(Literal),
}

impl TryFrom<&String> for DecoratorFlag {
    type Error = ();

    fn try_from(value: &String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "calldata" => Ok(DecoratorFlag::Calldata(String::default())),
            "value" => Ok(DecoratorFlag::Value(Literal::default())),
            _ => Err(()),
        }
    }
}

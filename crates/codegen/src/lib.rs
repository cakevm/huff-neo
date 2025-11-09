#![doc = include_str!("../README.md")]
#![warn(missing_docs)]
#![warn(unused_extern_crates)]
#![forbid(unsafe_code)]

use crate::irgen::builtin_function::builtin_pad;
use alloy_primitives::hex;
use huff_neo_utils::ast::huff::*;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::bytes_util::str_to_bytes32;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::scope::ScopeManager;
use huff_neo_utils::{
    abi::*,
    artifact::*,
    bytecode::*,
    bytes_util,
    error::CodegenError,
    opcodes::Opcode,
    prelude::{CodegenErrorKind, EVMVersion, PushValue, Span, format_even_bytes, pad_n_bytes},
    types::EToken,
};
use regex::Regex;
use std::{cmp::Ordering, collections::HashMap, fs, path::Path, sync::Arc};

mod irgen;
use crate::irgen::prelude::*;

/// Maximum macro recursion depth allowed during compilation.
///
/// This prevents infinite recursion in macro expansion or argument resolution,
/// which can occur with circular macro calls or recursive argument references.
pub(crate) const MAX_MACRO_RECURSION_DEPTH: usize = 100;

/// Maximum number of iterations allowed for the jump relaxation optimization algorithm.
///
/// This limit prevents infinite loops in pathological cases where convergence fails,
/// though typical contracts converge in a few iterations.
const JUMP_RELAXATION_MAX_ITERATIONS: usize = 20;

/// Compiles Huff contracts into EVM bytecode.
///
/// Transforms a Contract AST into executable bytecode, handling macro expansion,
/// label resolution, and bytecode optimization.
///
/// ```rust
/// use huff_neo_codegen::Codegen;
/// let cg = Codegen::new();
/// ```
#[derive(Debug, Default, Clone)]
pub struct Codegen {
    /// The Input AST
    pub ast: Option<Contract>,
    /// A cached codegen output artifact
    pub artifact: Option<Artifact>,
    /// Intermediate main bytecode store
    pub main_bytecode: Option<String>,
    /// Intermediate constructor bytecode store
    pub constructor_bytecode: Option<String>,
}

impl Codegen {
    /// Public associated function to instantiate a new Codegen instance.
    pub fn new() -> Self {
        Self { ast: None, artifact: None, main_bytecode: None, constructor_bytecode: None }
    }

    /// Compiles the contract's main macro into executable bytecode.
    ///
    /// # Arguments
    ///
    /// * `evm_version` - Target EVM version
    /// * `contract` - The contract to compile
    /// * `alternative_main` - Optional alternative entry point (defaults to "MAIN")
    /// * `relax_jumps` - Enable jump optimization to reduce bytecode size
    pub fn generate_main_bytecode(
        evm_version: &EVMVersion,
        contract: &Contract,
        alternative_main: Option<String>,
        relax_jumps: bool,
    ) -> Result<String, CodegenError> {
        let (bytecode, _source_map) = Self::generate_main_bytecode_with_sourcemap(evm_version, contract, alternative_main, relax_jumps)?;
        Ok(bytecode)
    }

    /// Compiles the contract's main macro into bytecode with source map.
    ///
    /// Like `generate_main_bytecode`, but also returns a source map for debugging.
    pub fn generate_main_bytecode_with_sourcemap(
        evm_version: &EVMVersion,
        contract: &Contract,
        alternative_main: Option<String>,
        relax_jumps: bool,
    ) -> Result<(String, Vec<SourceMapEntry>), CodegenError> {
        // Update table sizes
        let contract = Self::update_table_size(evm_version, contract)?;

        // If an alternative main is provided, then use it as the compilation target
        let main_macro = alternative_main.unwrap_or_else(|| String::from("MAIN"));

        // Find the main macro
        let m_macro = Codegen::get_macro_by_name(&main_macro, &contract)?;

        // Create scope manager and push the main macro
        let mut scope_mgr = ScopeManager::new();
        scope_mgr.push_macro(m_macro, 0);

        // For each MacroInvocation Statement, recurse into bytecode
        let bytecode_res: BytecodeRes =
            Codegen::macro_to_bytecode(evm_version, m_macro, &contract, &mut scope_mgr, 0, false, None, relax_jumps)?;

        tracing::debug!(target: "codegen", "Generated main bytecode. Appending table bytecode...");

        // Generate the fully baked bytecode
        Codegen::gen_table_bytecode(&contract, bytecode_res)
    }

    /// Generates constructor bytecode from a Contract AST
    pub fn generate_constructor_bytecode(
        evm_version: &EVMVersion,
        contract: &Contract,
        alternative_constructor: Option<String>,
        relax_jumps: bool,
    ) -> Result<(String, bool), CodegenError> {
        let ((bytecode, _source_map), has_custom) =
            Self::generate_constructor_bytecode_with_sourcemap(evm_version, contract, alternative_constructor, relax_jumps)?;
        Ok((bytecode, has_custom))
    }

    /// Generates constructor bytecode with source map from a Contract AST
    pub fn generate_constructor_bytecode_with_sourcemap(
        evm_version: &EVMVersion,
        contract: &Contract,
        alternative_constructor: Option<String>,
        relax_jumps: bool,
    ) -> Result<((String, Vec<SourceMapEntry>), bool), CodegenError> {
        // Update table sizes
        let contract = Self::update_table_size(evm_version, contract)?;

        // If an alternative constructor macro is provided, then use it as the compilation target
        let constructor_macro = alternative_constructor.unwrap_or_else(|| String::from("CONSTRUCTOR"));

        // Find the constructor macro
        let c_macro = Codegen::get_macro_by_name(&constructor_macro, &contract)?;

        // Create scope manager and push the constructor macro
        let mut scope_mgr = ScopeManager::new();
        scope_mgr.push_macro(c_macro, 0);

        // For each MacroInvocation Statement, recurse into bytecode
        let bytecode_res: BytecodeRes =
            Codegen::macro_to_bytecode(evm_version, c_macro, &contract, &mut scope_mgr, 0, false, None, relax_jumps)?;

        // Check if the constructor performs its own code generation
        let has_custom_bootstrap = bytecode_res.bytes.iter().any(|seg| seg.bytes.as_str() == "f3");

        tracing::info!(target: "codegen", "Constructor is self-generating: {}", has_custom_bootstrap);

        let (bytecode, source_map) = Codegen::gen_table_bytecode(&contract, bytecode_res)?;

        Ok(((bytecode, source_map), has_custom_bootstrap))
    }

    /// Update the table size in the contract that was not know at the time of parsing
    pub fn update_table_size(_evm_version: &EVMVersion, contract: &Contract) -> Result<Contract, CodegenError> {
        // Update the table size from generated bytecode
        let mut updated = vec![];
        for table_definition in contract.tables.iter() {
            if table_definition.size.is_some() {
                updated.push(table_definition.clone());
                continue;
            }
            // If the size is not provided we need to generate the bytecode to know the size
            let byte_code = Codegen::gen_table_bytecode_builtin(contract, table_definition)?;
            let size = str_to_bytes32(format!("{:02x}", byte_code.len() / 2).as_str());
            let mut table = table_definition.clone();
            table.size = Some(size);
            updated.push(table);
        }
        let mut contract = contract.clone();
        contract.tables = updated;
        Ok(contract)
    }

    /// Helper function to find a macro or generate a CodegenError
    pub(crate) fn get_macro_by_name<'a>(name: &str, contract: &'a Contract) -> Result<&'a MacroDefinition, CodegenError> {
        if let Some(m) = contract.find_macro_by_name(name) {
            Ok(m)
        } else {
            tracing::error!(target: "codegen", "MISSING \"{}\" MACRO!", name);
            Err(CodegenError {
                kind: CodegenErrorKind::MissingMacroDefinition(name.to_string()),
                span: AstSpan(vec![Span { start: 0, end: 0, file: None }]).boxed(),
                token: None,
            })
        }
    }

    /// Generates bytecode for a builtin function call used for constants, code tables, etc.
    /// Returns a PushValue that can be converted to bytecode with or without opcode
    pub fn gen_builtin_bytecode(contract: &Contract, bf: &BuiltinFunctionCall, span: AstSpan) -> Result<PushValue, CodegenError> {
        use huff_neo_utils::builtin_eval::{PadDirection, eval_builtin_bytes, eval_event_hash, eval_function_signature};
        match bf.kind {
            BuiltinFunctionKind::FunctionSignature => eval_function_signature(contract, bf),
            BuiltinFunctionKind::Bytes => eval_builtin_bytes(bf),
            BuiltinFunctionKind::EventHash => eval_event_hash(contract, bf),
            BuiltinFunctionKind::LeftPad => builtin_pad(contract, bf, PadDirection::Left),
            BuiltinFunctionKind::RightPad => builtin_pad(contract, bf, PadDirection::Right),
            _ => Err(CodegenError {
                kind: CodegenErrorKind::UnsupportedBuiltinFunction(format!("{}", bf.kind)),
                span: span.boxed(),
                token: None,
            }),
        }
    }

    /// Generates bytecode for a code table with builtin functions
    pub fn gen_table_bytecode_builtin(contract: &Contract, table_definition: &TableDefinition) -> Result<String, CodegenError> {
        let mut byte_code = String::new();
        for statement in table_definition.statements.clone() {
            match &statement.ty {
                StatementType::Code(code) => {
                    byte_code = format!("{byte_code}{code}");
                }
                StatementType::BuiltinFunctionCall(bf) => {
                    let push_value = Codegen::gen_builtin_bytecode(contract, bf, statement.span.clone())?;
                    // Use full hex for padding functions (always 32 bytes), trimmed for others
                    let hex_data = match bf.kind {
                        BuiltinFunctionKind::LeftPad | BuiltinFunctionKind::RightPad => push_value.to_hex_full(),
                        _ => push_value.to_hex_trimmed(),
                    };
                    byte_code = format!("{byte_code}{hex_data}")
                }
                StatementType::Constant(name) => {
                    let constant = lookup_constant(name, contract, &statement.span)?;
                    let bytes = match &constant.value {
                        ConstVal::Bytes(bytes) => bytes.clone(),
                        ConstVal::Expression(expr) => {
                            let literal = contract.evaluate_constant_expression(expr)?;
                            Bytes::Raw(bytes_util::bytes32_to_hex_string(&literal, false))
                        }
                        ConstVal::BuiltinFunctionCall(bf) => {
                            let push_value = Codegen::gen_builtin_bytecode(contract, bf, statement.span.clone())?;
                            // Use full hex for padding functions (always 32 bytes), trimmed for others
                            let hex_data = match bf.kind {
                                BuiltinFunctionKind::LeftPad | BuiltinFunctionKind::RightPad => push_value.to_hex_full(),
                                _ => push_value.to_hex_trimmed(),
                            };
                            Bytes::Raw(hex_data)
                        }
                        _ => {
                            return Err(CodegenError {
                                kind: CodegenErrorKind::InvalidArguments("Constant must be a hex literal or expression".to_string()),
                                span: statement.span.clone_box(),
                                token: None,
                            });
                        }
                    };
                    byte_code = format!("{byte_code}{}", bytes.as_str());
                }
                _ => {
                    return Err(CodegenError {
                        kind: CodegenErrorKind::UnsupportedStatementType(format!("{}", statement.ty)),
                        span: table_definition.span.clone_box(),
                        token: None,
                    });
                }
            }
        }
        Ok(byte_code)
    }

    /// Appends table bytecode to the end of the BytecodeRes output.
    /// Fills table JUMPDEST placeholders.
    /// Returns both the bytecode string and the source map
    pub fn gen_table_bytecode(contract: &Contract, res: BytecodeRes) -> Result<(String, Vec<SourceMapEntry>), CodegenError> {
        if !res.unmatched_jumps.is_empty() {
            let labels = res.unmatched_jumps.iter().map(|uj| uj.label.to_string()).collect::<Vec<String>>();
            tracing::error!(
                target: "codegen",
                "Source contains unmatched jump labels \"{}\"",
                labels.join(", ")
            );

            return Err(CodegenError {
                kind: CodegenErrorKind::UnmatchedJumpLabels(labels),
                span: AstSpan(res.unmatched_jumps.iter().flat_map(|uj| uj.span.0.clone()).collect::<Vec<Span>>()).boxed(),
                token: None,
            });
        }

        tracing::info!(target: "codegen", "GENERATING JUMPTABLE BYTECODE");
        tracing::debug!(target: "codegen", "Contract has {} source files", contract.source_files.len());
        tracing::debug!(target: "codegen", "Source map entries: {:?}", contract.source_map);
        tracing::debug!(target: "codegen", "res.bytes.len() = {}", res.bytes.len());
        tracing::debug!(target: "codegen", "res.spans.len() = {}", res.spans.len());

        // Generate source map from bytecode segments and spans
        // Create one entry per instruction for Solidity-compatible source maps
        // The source map is indexed by IC, so we need an entry for every instruction
        let mut source_map = Vec::new();
        let mut program_counter = 0; // PC: byte offset in bytecode

        for (instruction_counter, seg) in res.bytes.iter().enumerate() {
            let instruction_bytecode_length = seg.bytes.len(); // Length in bytes

            // Create source map entry for this instruction
            // Use span information if available, otherwise use zeros (unmapped)
            let (source_start, source_end, file_id) = if let Some(Some((start, end))) = res.spans.get(instruction_counter) {
                // Use the Contract's helper method to map flattened positions to original file positions
                let (file_id, original_start, original_end) = contract.map_flattened_position_to_source(*start, *end);

                tracing::debug!(target: "codegen", "Source map entry: IC={}, flattened_pos={}..{}, mapped_pos={}..{}, file={}, bytes={}, PC={}, bytecode_len={}, source_len={}",
                    instruction_counter, start, end, original_start, original_end, file_id, seg.bytes.as_str(), program_counter, instruction_bytecode_length, original_end - original_start);
                (original_start, original_end, file_id)
            } else {
                // No source mapping for this instruction
                tracing::debug!(target: "codegen", "Source map entry: IC={}, no span, using (0,0,0)", instruction_counter);
                (0, 0, 0)
            };

            source_map.push(SourceMapEntry {
                ic: instruction_counter,                      // IC: instruction index (0, 1, 2, ...)
                pc: program_counter,                          // PC: byte offset in bytecode where this instruction starts
                bytecode_length: instruction_bytecode_length, // Length of this instruction in bytecode (bytes)
                source_start,
                source_length: source_end - source_start, // Length in source code (chars, not bytecode bytes)
                file_id,
            });

            program_counter += instruction_bytecode_length; // Advance PC by the bytecode length
        }

        let mut bytecode = res.bytes.into_iter().map(|seg| seg.bytes.as_str().to_string()).collect::<String>();
        let mut table_offsets: HashMap<String, usize> = HashMap::new(); // table name -> bytecode offset
        let mut table_offset = bytecode.len() / 2;

        res.utilized_tables.iter().try_for_each(|jt| {
            // Skip tables that were embedded inline
            if res.embedded_tables.contains_key(&jt.name) {
                tracing::debug!(target: "codegen", "Skipping embedded table \"{}\" from end-placement", jt.name);
                return Ok(());
            }

            table_offsets.insert(jt.name.to_string(), table_offset);

            let Some(table_size) = &jt.size else {
                return Err(CodegenError {
                    kind: CodegenErrorKind::MissingTableSize(jt.name.clone()),
                    span: jt.span.clone_box(),
                    token: None,
                });
            };

            let size = match bytes_util::hex_to_usize(bytes_util::bytes32_to_hex_string(table_size, false).as_str()) {
                Ok(s) => s,
                Err(e) => {
                    tracing::error!(target: "codegen", "Errored converting bytes32 to str. Bytes {:?} with error: {:?}", jt.size, e);
                    return Err(CodegenError {
                        kind: CodegenErrorKind::UsizeConversion(format!("{:?}", jt.size)),
                        span: jt.span.clone_box(),
                        token: None,
                    });
                }
            };
            table_offset += size;

            if jt.kind == TableKind::CodeTable {
                let table_code = Codegen::gen_table_bytecode_builtin(contract, jt)?;

                bytecode = format!("{bytecode}{table_code}");
                return Ok(());
            }

            // For all other tables
            tracing::info!(target: "codegen", "GENERATING BYTECODE FOR TABLE: \"{}\"", jt.name);

            let mut table_code = String::new();
            jt.statements.iter().try_for_each(|s| {
                match &s.ty {
                    StatementType::LabelCall(label) => {
                        // For jump table label lookups, try to find the label in any scope
                        // Jump tables are generated at the top level but may reference labels in inner scopes
                        let offset = match res.label_indices.get_any(label) {
                            Some(l) => l,
                            None => {
                                tracing::error!(
                                    target: "codegen",
                                    "Definition not found for Jump Table Label: \"{}\"",
                                    label
                                );
                                return Err(CodegenError {
                                    kind: CodegenErrorKind::UnmatchedJumpLabels(vec![label.clone()]),
                                    span: s.span.clone_box(),
                                    token: None,
                                });
                            }
                        };
                        let hex = format_even_bytes(format!("{offset:02x}"));

                        table_code = format!(
                            "{table_code}{}",
                            pad_n_bytes(hex.as_str(), if matches!(jt.kind, TableKind::JumpTablePacked) { 0x02 } else { 0x20 },)
                        );
                    }
                    _ => {
                        return Err(CodegenError { kind: CodegenErrorKind::InvalidMacroStatement, span: jt.span.clone_box(), token: None });
                    }
                }
                Ok(())
            })?;
            tracing::info!(target: "codegen", "SUCCESSFULLY GENERATED BYTECODE FOR TABLE: \"{}\"", jt.name);
            bytecode = format!("{bytecode}{table_code}");
            Ok(())
        })?;

        res.table_instances.iter().for_each(|jump| {
            // Check both end-placed tables and embedded tables
            let offset_opt = table_offsets.get(&jump.label).or_else(|| res.embedded_tables.get(&jump.label));

            if let Some(o) = offset_opt {
                let before = &bytecode[0..jump.bytecode_index * 2 + 2];
                let after = &bytecode[jump.bytecode_index * 2 + 6..];

                bytecode = format!("{before}{}{after}", pad_n_bytes(format!("{o:02x}").as_str(), 2));
                tracing::info!(target: "codegen", "FILLED JUMPDEST FOR LABEL \"{}\"", jump.label);
            } else {
                tracing::error!(
                    target: "codegen",
                    "Jump table offset not present for jump label \"{}\"",
                    jump.label
                );
            }
        });

        Ok((bytecode, source_map))
    }

    /// Generates bytecode from a macro definition.
    ///
    /// Expands the macro body into bytecode, resolving labels, arguments, and nested macro calls.
    ///
    /// # Arguments
    ///
    /// * `evm_version` - Target EVM version for opcode compatibility
    /// * `macro_def` - The macro to compile
    /// * `contract` - Contract context for macro and constant lookups
    /// * `scope_mgr` - Call stack for tracking macro invocations and scope
    /// * `offset` - Starting bytecode offset
    /// * `recursing_constructor` - Whether compiling within a constructor
    /// * `circular_codesize_invocations` - Tracks __CODESIZE recursion to prevent infinite loops
    /// * `relax_jumps` - Enable jump size optimization
    ///
    /// # Returns
    ///
    /// Bytecode with label indices, unresolved jumps, and source map information.
    #[allow(clippy::too_many_arguments)]
    pub fn macro_to_bytecode<'a>(
        evm_version: &EVMVersion,
        macro_def: &'a MacroDefinition,
        contract: &'a Contract,
        scope_mgr: &mut ScopeManager<'a>,
        mut offset: usize,
        recursing_constructor: bool,
        circular_codesize_invocations: Option<&mut CircularCodeSizeIndices>,
        relax_jumps: bool,
    ) -> Result<BytecodeRes, CodegenError> {
        // Safety check to prevent infinite recursion
        if scope_mgr.depth() > MAX_MACRO_RECURSION_DEPTH {
            let macro_chain = scope_mgr.macro_stack().iter().map(|m| m.name.as_str()).collect::<Vec<_>>().join(" -> ");
            return Err(CodegenError {
                kind: CodegenErrorKind::InvalidMacroArgumentType(format!(
                    "Maximum macro recursion depth ({}) exceeded while compiling macro '{}'.\nMacro call chain: {}\nThis likely indicates circular macro calls or infinitely recursive macro expansion.",
                    MAX_MACRO_RECURSION_DEPTH, macro_def.name, macro_chain
                )),
                span: macro_def.span.clone_box(),
                token: None,
            });
        }

        // Get intermediate bytecode representation of the macro definition
        let mut bytes = BytecodeSegments::new();
        let mut spans: Vec<Option<(usize, usize)>> = Vec::default();
        let ir_bytes = macro_def.to_irbytecode(evm_version)?.0;

        // Define outer loop variables
        let mut jump_table = JumpTable::new();
        let mut label_indices = LabelIndices::new();
        let mut table_instances = Jumps::new();
        let mut utilized_tables: Vec<TableDefinition> = Vec::new();
        let mut embedded_tables = std::collections::BTreeMap::new();
        let mut ccsi = CircularCodeSizeIndices::new();
        let circular_codesize_invocations = circular_codesize_invocations.unwrap_or(&mut ccsi);

        // Loop through all intermediate bytecode representations generated from the AST
        for ir_byte in ir_bytes.iter() {
            let starting_offset = offset;

            // Extract span information if available
            // Convert from original file coordinates to flattened coordinates
            let span_info = if !ir_byte.span.0.is_empty() {
                ir_byte.span.0.first().and_then(|s| {
                    if s.start != 0 || s.end != 0 {
                        // Convert original span to flattened coordinates
                        contract.map_original_span_to_flattened(s)
                    } else {
                        None
                    }
                })
            } else {
                None
            };

            match &ir_byte.ty {
                IRByteType::Bytes(b) => {
                    offset += b.len(); // len() now returns byte count directly
                    bytes.push_with_offset(starting_offset, b.to_owned());
                    spans.push(span_info);
                }
                IRByteType::Constant(name) => {
                    if let Some(push_value) = constant_gen(name, contract, ir_byte.span)? {
                        let push_bytes = push_value.to_hex_with_opcode(evm_version);
                        offset += push_bytes.len() / 2;
                        tracing::debug!(target: "codegen", "OFFSET: {}, PUSH BYTES: {:?}", offset, push_bytes);
                        bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
                        spans.push(span_info);
                    } // __NOOP generates no bytecode
                }
                IRByteType::Statement(s) => {
                    // if we have a codesize call for the constructor here, from within the
                    // constructor, we skip
                    if recursing_constructor {
                        continue;
                    }
                    let (push_bytes, mut push_spans) = statement_gen(
                        evm_version,
                        s,
                        contract,
                        macro_def,
                        scope_mgr,
                        &mut offset,
                        &mut jump_table,
                        &mut label_indices,
                        &mut table_instances,
                        &mut utilized_tables,
                        &mut embedded_tables,
                        circular_codesize_invocations,
                        starting_offset,
                        relax_jumps,
                    )?;
                    // Use the spans from statement_gen (which preserves nested macro spans)
                    // instead of duplicating the parent's span
                    spans.append(&mut push_spans);
                    bytes.extend(push_bytes);
                }
                IRByteType::ArgCall(parent_macro_name, arg_name) => {
                    let bytes_before = bytes.len();
                    // Bubble up arg call by looking through the previous scopes.
                    // Once the arg value is found, add it to `bytes`
                    bubble_arg_call(
                        evm_version,
                        parent_macro_name.to_owned(),
                        arg_name.to_owned(),
                        &mut bytes,
                        macro_def,
                        contract,
                        scope_mgr,
                        &mut offset,
                        &mut jump_table,
                        &mut table_instances,
                        &mut utilized_tables,
                        ir_byte.span,
                        relax_jumps,
                    )?;
                    // Add span for each byte segment added by bubble_arg_call
                    let bytes_added = bytes.len() - bytes_before;
                    for _ in 0..bytes_added {
                        spans.push(span_info);
                    }
                }
            }
        }

        // Note: We don't pop from scope_mgr here because it's either:
        // 1. Done by the caller after the recursive call returns (for inline macros)
        // 2. Done at the end of this function (for top-level or function generation)

        // Add functions (outlined macros) to the end of the bytecode if the scope length == 1
        // (i.e., we're at the top level of recursion)
        if scope_mgr.depth() == 1 {
            let bytes_before = bytes.len();
            bytes = Codegen::append_functions(
                evm_version,
                contract,
                scope_mgr,
                &mut offset,
                &mut jump_table,
                &mut label_indices,
                &mut table_instances,
                bytes,
            )?;
            // Add empty spans for appended functions (these don't have direct source mapping)
            let bytes_added = bytes.len() - bytes_before;
            for _ in 0..bytes_added {
                spans.push(None);
            }
        } else {
            // If the scope length is > 1, we're processing a child macro. Since we're done
            // with it, it can be popped.
            scope_mgr.pop_macro();
        }

        // Fill JUMPDEST placeholders
        let (new_bytes, unmatched_jumps, updated_label_indices) =
            Codegen::fill_unmatched(bytes.clone(), &jump_table, &label_indices, relax_jumps)?;
        let label_indices = updated_label_indices; // Use the updated indices (which account for relaxation)

        // Adjust spans if bytes changed
        if new_bytes.len() != bytes.len() {
            // If the byte count changed, we need to rebuild the spans vector
            // For now, just ensure it has the same length
            while spans.len() < new_bytes.len() {
                spans.push(None);
            }
        }
        let bytes = new_bytes;

        // Fill in circular codesize invocations
        // Workout how to increase the offset the correct amount within here if it is longer than 2
        // bytes
        let new_bytes = Codegen::fill_circular_codesize_invocations(bytes.clone(), circular_codesize_invocations, &macro_def.name)?;

        // Adjust spans if bytes changed
        if new_bytes.len() != bytes.len() {
            while spans.len() < new_bytes.len() {
                spans.push(None);
            }
        }
        let bytes = new_bytes;

        tracing::info!(target: "codegen", "BytecodeRes created with {} bytes and {} spans", bytes.len(), spans.len());
        Ok(BytecodeRes { bytes, spans, label_indices, unmatched_jumps, table_instances, utilized_tables, embedded_tables })
    }

    /// Optimize PUSH2 (2-byte) jumps to PUSH1 (1-byte) jumps where possible.
    ///
    /// ## Overview
    ///
    /// Implements Szymanski's "Start Big and Shrink" algorithm for jump relaxation.
    /// Iteratively attempts to shrink PUSH2 (2-byte) jumps to PUSH1 (1-byte) jumps
    /// when the target label is within 0-255 byte range.
    ///
    /// The algorithm:
    /// 1. Starts with all jumps as PUSH2 (2 bytes)
    /// 2. Reads existing label positions from `label_indices` (populated during codegen)
    /// 3. Attempts to shrink PUSH2 → PUSH1 where target ≤ 0xFF
    /// 4. Updates segment byte offsets and repeats until convergence or max iterations
    ///
    /// Jump relaxation using iterative offset recalculation ("Start big and shrink" approach)
    /// for span-dependent instructions. Based on techniques described in:
    /// Thomas G. Szymanski, "Assembling Code for Machines with Span-Dependent Instructions",
    /// CACM 21(4), 1978, p. 300-308.
    /// See also: <https://www.complang.tuwien.ac.at/anton/assembling-span-dependent.html>
    ///
    /// This is a simpler iterative variant of Szymanski's original graph-based algorithm.
    /// It iteratively optimizes jump instructions from PUSH2 to PUSH1 when targets fit
    /// within 0-255 bytes. Each iteration updates segment byte offsets since shrinking one
    /// jump affects the position of all subsequent segments.
    ///
    /// ## Arguments
    /// * `bytes` - The bytecode segments containing jump placeholders
    /// * `jump_table` - Table mapping offsets to Jump metadata
    /// * `label_indices` - Scoped label positions
    ///
    /// ## Returns
    /// * `Ok((BytecodeSegments, LabelIndices, offset_mapping))` - Optimized bytecode, updated label indices, and mapping from original to new offsets
    /// * `Err(CodegenError)` - If optimization fails
    pub fn relax_jump_offsets(
        mut bytes: BytecodeSegments,
        jump_table: &JumpTable,
        label_indices: &LabelIndices,
    ) -> Result<(BytecodeSegments, LabelIndices, std::collections::BTreeMap<usize, usize>), CodegenError> {
        let mut iteration = 0;
        let mut changed = true; // Did any changes occur in the last iteration?
        let mut current_label_indices = label_indices.clone(); // Mutable copy that we update each iteration
        // Track the cumulative mapping from original offsets to final offsets
        let mut cumulative_offset_mapping: std::collections::BTreeMap<usize, usize> = std::collections::BTreeMap::new();

        tracing::info!(target: "codegen", "Starting jump relaxation optimization");

        while changed && iteration < JUMP_RELAXATION_MAX_ITERATIONS {
            changed = false;
            iteration += 1;

            // Calculate current segment byte offsets (accounting for any size changes)
            let offsets = bytes.calculate_offsets();

            tracing::debug!(target: "codegen", "Relaxation iteration {} - checking {} segments", iteration, bytes.len());

            // Iterate through all segments looking for jump placeholders
            for (segment_idx, segment) in bytes.iter_mut().enumerate() {
                if let Bytes::JumpPlaceholder(ref mut placeholder) = segment.bytes {
                    // Only optimize if currently PUSH2
                    if placeholder.push_opcode != PushOpcode::Push2 {
                        continue;
                    }

                    // Get the current bytecode offset for this segment
                    let current_offset = offsets[segment_idx];

                    // Find the corresponding jump in the jump table
                    if let Some(jumps) = jump_table.get(&segment.offset) {
                        // Use the first jump (typically there's only one per offset)
                        if let Some(jump) = jumps.first() {
                            // Look up the target label's offset using the CURRENT iteration's label indices
                            match current_label_indices.get(&placeholder.label, &jump.scope_id) {
                                Ok(Some(target_offset)) => {
                                    // Check if target fits in PUSH1 (0-255)
                                    // OR if shrinking this jump would bring a forward target into PUSH1 range
                                    let can_use_push1 = if target_offset <= 0xFF {
                                        // Target already fits in PUSH1
                                        true
                                    } else if target_offset == 0x100 && target_offset > current_offset {
                                        // Special case: target is at exactly 256 (0x100) and is a forward jump
                                        // Shrinking PUSH2->PUSH1 saves 1 byte, moving target from 256 to 255
                                        tracing::debug!(
                                            target: "codegen",
                                            "Forward jump to '{}' at 256 will fit in PUSH1 after relaxation",
                                            placeholder.label
                                        );
                                        true
                                    } else {
                                        false
                                    };

                                    if can_use_push1 {
                                        tracing::debug!(
                                            target: "codegen",
                                            "Shrinking jump to '{}' at offset {} from PUSH2 to PUSH1 (target={})",
                                            placeholder.label,
                                            current_offset,
                                            target_offset
                                        );

                                        // Shrink from PUSH2 to PUSH1
                                        placeholder.push_opcode = PushOpcode::Push1;
                                        changed = true;
                                    } else {
                                        tracing::trace!(
                                            target: "codegen",
                                            "Jump to '{}' at offset {} remains PUSH2 (target={})",
                                            placeholder.label,
                                            current_offset,
                                            target_offset
                                        );
                                    }
                                }
                                Ok(None) => {
                                    // Label not found - will be handled by fill_unmatched later
                                    tracing::trace!(
                                        target: "codegen",
                                        "Label '{}' not found during relaxation, will be resolved later",
                                        placeholder.label
                                    );
                                }
                                Err(e) => {
                                    // Error looking up label - will be handled by fill_unmatched later
                                    tracing::trace!(
                                        target: "codegen",
                                        "Error looking up label '{}' during relaxation: {:?}",
                                        placeholder.label,
                                        e
                                    );
                                }
                            }
                        }
                    }
                }
            }

            if changed {
                tracing::debug!(target: "codegen", "Iteration {} made changes, will recalculate label positions", iteration);

                // Recalculate all positions based on current segment sizes
                let new_offsets = bytes.calculate_offsets();

                // Build offset mapping for this iteration
                let mut offset_mapping: std::collections::BTreeMap<usize, usize> = std::collections::BTreeMap::new();
                for (segment_idx, segment) in bytes.iter().enumerate() {
                    offset_mapping.insert(segment.offset, new_offsets[segment_idx]);
                }

                // Update the cumulative mapping
                // For each original offset, update it to point to the latest calculated offset
                if cumulative_offset_mapping.is_empty() {
                    // First iteration: just copy the mapping
                    cumulative_offset_mapping = offset_mapping.clone();
                } else {
                    // Subsequent iterations: chain the mappings
                    // If we had original->iter1, and now have iter1->iter2, we want original->iter2
                    for intermediate_offset in cumulative_offset_mapping.values_mut() {
                        if let Some(&new_offset) = offset_mapping.get(intermediate_offset) {
                            *intermediate_offset = new_offset;
                        }
                    }
                }

                // Update the label indices using this iteration's mapping
                current_label_indices.update_offsets(&offset_mapping);

                // Update segment offsets for next iteration
                for (segment_idx, segment) in bytes.iter_mut().enumerate() {
                    segment.offset = new_offsets[segment_idx];
                }
            }
        }

        if iteration >= JUMP_RELAXATION_MAX_ITERATIONS {
            tracing::warn!(target: "codegen", "Jump relaxation hit max iterations ({})", JUMP_RELAXATION_MAX_ITERATIONS);
        } else {
            tracing::info!(target: "codegen", "Jump relaxation converged after {} iterations", iteration);
        }

        Ok((bytes, current_label_indices, cumulative_offset_mapping))
    }

    /// Resolves jump placeholders to their target offsets with optional size optimization.
    ///
    /// Replaces jump placeholders with actual bytecode offsets by looking up labels in the
    /// label index. When `relax_jumps` is enabled, optimizes PUSH2 jumps to PUSH1 where
    /// targets fit within a single byte (0-255).
    ///
    /// ## Arguments
    ///
    /// * `bytes` - Bytecode segments with jump placeholders
    /// * `jump_table` - Jump metadata indexed by bytecode offset
    /// * `label_indices` - Scoped label positions
    /// * `relax_jumps` - Enable jump size optimization
    ///
    /// ## Returns
    ///
    /// Returns resolved bytecode and any unmatched jumps, or an error for label conflicts
    /// or invalid jump targets.
    pub fn fill_unmatched(
        mut bytes: BytecodeSegments,
        jump_table: &JumpTable,
        label_indices: &LabelIndices,
        relax_jumps: bool,
    ) -> Result<(BytecodeSegments, Vec<Jump>, LabelIndices), CodegenError> {
        // Apply jump relaxation optimization if enabled
        // This will replace PUSH2 jumps with PUSH1 where possible, before final resolution
        let jump_table_to_use: std::collections::BTreeMap<usize, Vec<Jump>>;
        let label_indices_to_use: LabelIndices;
        let jump_table_ref: &JumpTable;
        let label_indices_ref: &LabelIndices;

        if relax_jumps {
            // Perform jump relaxation, which updates bytecode, label indices, and returns offset mapping
            let (relaxed_bytes, relaxed_label_indices, offset_mapping) = Self::relax_jump_offsets(bytes, jump_table, label_indices)?;
            bytes = relaxed_bytes;

            // Rebuild the jump_table using the offset mapping from relaxation
            jump_table_to_use = jump_table
                .iter()
                .filter_map(|(old_offset, jumps)| offset_mapping.get(old_offset).map(|&new_offset| (new_offset, jumps.clone())))
                .collect();

            // Use the label indices returned from relaxation (already updated)
            label_indices_to_use = relaxed_label_indices;

            jump_table_ref = &jump_table_to_use;
            label_indices_ref = &label_indices_to_use;
        } else {
            jump_table_ref = jump_table;
            label_indices_ref = label_indices;
            label_indices_to_use = label_indices.clone();
        }

        // Resolve all jumps using the new typed approach
        let unmatched_jumps = bytes.resolve_jumps(jump_table_ref, label_indices_ref).map_err(|label_error| {
            // Match on the typed error variants
            match label_error {
                LabelError::DuplicateLabelAcrossSiblings(label_name) => {
                    // Find the first jump to get its span
                    let span = jump_table
                        .values()
                        .flat_map(|jumps| jumps.iter())
                        .find(|jump| jump.label == label_name)
                        .map(|jump| jump.span.clone_box())
                        .unwrap_or_else(|| Box::new(AstSpan::default()));

                    CodegenError { kind: CodegenErrorKind::DuplicateLabelAcrossSiblings(label_name), span, token: None }
                }
                LabelError::JumpTargetTooLarge { label, target, opcode } => {
                    // Find the jump to get its span
                    let span = jump_table
                        .values()
                        .flat_map(|jumps| jumps.iter())
                        .find(|jump| jump.label == label)
                        .map(|jump| jump.span.clone_box())
                        .unwrap_or_else(|| Box::new(AstSpan::default()));

                    CodegenError { kind: CodegenErrorKind::JumpTargetTooLarge { label, target, opcode }, span, token: None }
                }
                LabelError::DuplicateLabelInScope(label_name) => {
                    // This shouldn't happen during jump resolution, but handle it anyway
                    let span = jump_table
                        .values()
                        .flat_map(|jumps| jumps.iter())
                        .find(|jump| jump.label == label_name)
                        .map(|jump| jump.span.clone_box())
                        .unwrap_or_else(|| Box::new(AstSpan::default()));

                    CodegenError { kind: CodegenErrorKind::DuplicateLabelInScope(label_name), span, token: None }
                }
            }
        })?;

        // Log unmatched jumps
        for jump in &unmatched_jumps {
            tracing::warn!(
                target: "codegen",
                "UNMATCHED JUMP LABEL \"{}\" AT BYTECODE INDEX {} (scope_id={:?})",
                jump.label, jump.bytecode_index, jump.scope_id
            );
        }

        // Validate __ASSERT_PC placeholders using post-relaxation offsets
        // This must happen AFTER jump relaxation to ensure correct offset validation
        // The segment.offset has been updated by relaxation, so we can use it directly
        let offsets = bytes.calculate_offsets();
        for (idx, segment) in bytes.iter().enumerate() {
            if let Bytes::AssertPcPlaceholder(data) = &segment.bytes {
                // Use the calculated offset which accounts for all relaxation
                let actual_offset = offsets[idx];

                if actual_offset != data.expected_offset {
                    tracing::error!(
                        target: "codegen",
                        "PC assertion failed: expected 0x{:x}, got 0x{:x}",
                        data.expected_offset,
                        actual_offset
                    );
                    return Err(CodegenError {
                        kind: CodegenErrorKind::AssertPcFailed(data.expected_offset, actual_offset),
                        span: data.span.clone_box(),
                        token: None,
                    });
                }

                tracing::debug!(
                    target: "codegen",
                    "PC assertion passed: position is 0x{:x} as expected (after relaxation)",
                    actual_offset
                );
            }
        }

        // Filter out __ASSERT_PC placeholders since they don't generate bytecode
        let mut filtered_bytes = BytecodeSegments::new();
        for segment in bytes.iter() {
            if !matches!(segment.bytes, Bytes::AssertPcPlaceholder(_)) {
                filtered_bytes.push(segment.clone());
            }
        }

        Ok((filtered_bytes, unmatched_jumps, label_indices_to_use))
    }

    /// Helper associated function to fill circular codesize invocations.
    ///
    /// ## Overview
    ///
    /// This function should run after all other code generation has been completed.
    /// If there are placeholders for circular codesize invocations, this function will
    /// fill them in with the correct offset.
    ///
    /// If there are multiple invocations of the same macro, the function will take into
    /// account the total number of invocations and increase its offset accordingly.
    ///
    /// On success, returns a tuple of generated bytes.
    /// On failure, returns a CodegenError.
    pub fn fill_circular_codesize_invocations(
        mut bytes: BytecodeSegments,
        circular_codesize_invocations: &CircularCodeSizeIndices,
        macro_name: &str,
    ) -> Result<BytecodeSegments, CodegenError> {
        // Get the number of circular codesize invocations
        let num_invocations = circular_codesize_invocations.len();
        if num_invocations == 0 {
            return Ok(bytes);
        }

        tracing::debug!(target: "codegen", "Circular Codesize Invocation: Bytes before expansion: {:#?}", bytes);

        // Calculate initial size
        let length: usize = bytes.iter().map(|seg| seg.bytes.len()).sum::<usize>();

        // Determine if any placeholders will need to grow from PUSH1 to PUSH2
        // This is needed to calculate the correct final size
        let offset_increase = if length > 255 { 1 } else { 0 };
        // Codesize will increase by 1 byte for every codesize that grows to PUSH2
        let extended_length = length + (offset_increase * num_invocations);

        // Resolve all circular codesize placeholders with the calculated extended size
        let growth_offsets = bytes.resolve_circular_codesize(circular_codesize_invocations, macro_name, extended_length);

        // Apply offset adjustments for segments that come after grown placeholders
        for segment in bytes.iter_mut() {
            // Check if any growth happened before this segment
            let growth_before: usize =
                growth_offsets.iter().filter(|(growth_offset, _)| *growth_offset < segment.offset).map(|(_, growth)| growth).sum();

            if growth_before > 0 {
                // This segment comes after some growth, adjust its offset
                segment.offset += growth_before;
            }
        }

        tracing::debug!(
            target: "codegen",
            "Circular codesize resolution complete: {} placeholders resolved, {} bytes of growth, final size: {}",
            num_invocations,
            growth_offsets.iter().map(|(_, g)| g).sum::<usize>(),
            extended_length
        );

        Ok(bytes)
    }

    /// Helper associated function to append functions to the end of the bytecode.
    ///
    /// ## Overview
    ///
    /// Iterates over the contract's functions, generates their bytecode, fills unmatched jumps &
    /// label indices, and appends the functions' bytecode to the end of the contract's bytecode.
    ///
    /// On success, passes ownership of `bytes` back to the caller.
    /// On failure, returns a CodegenError.
    #[allow(clippy::too_many_arguments)]
    pub fn append_functions<'a>(
        evm_version: &EVMVersion,
        contract: &'a Contract,
        scope_mgr: &mut ScopeManager<'a>,
        offset: &mut usize,
        jump_table: &mut JumpTable,
        label_indices: &mut LabelIndices,
        table_instances: &mut Jumps,
        mut bytes: BytecodeSegments,
    ) -> Result<BytecodeSegments, CodegenError> {
        // First pass: Pre-register all function goto_ labels at their future offsets
        // This allows nested function calls to resolve correctly
        let functions: Vec<_> = contract.macros.values().filter(|m| m.outlined).collect();
        let mut current_offset = *offset;
        let goto_scope_id = huff_neo_utils::scope::ScopeId::new(vec![], 0);

        // We need to calculate offsets without actually generating bytecode yet
        // Store the calculated offset for each function
        let mut function_offsets: Vec<(&MacroDefinition, usize)> = Vec::new();

        for macro_def in &functions {
            // Register the goto_ label at the current offset
            if let Err(err_msg) = label_indices.insert(format!("goto_{}", macro_def.name.clone()), current_offset, &goto_scope_id) {
                // This shouldn't happen for auto-generated goto_ labels - indicates a compiler bug
                tracing::error!(target: "codegen", "INTERNAL ERROR: Duplicate goto_ label for function '{}': {}", macro_def.name, err_msg);
                return Err(CodegenError {
                    kind: CodegenErrorKind::DuplicateLabelInScope(format!("goto_{}", macro_def.name)),
                    span: macro_def.span.clone_box(),
                    token: None,
                });
            }

            function_offsets.push((macro_def, current_offset));

            // Estimate the size of this function for offset calculation
            // We'll need to generate it to get the exact size, but for now we need a placeholder
            // This is a chicken-and-egg problem: we need offsets to generate code, but need code to know offsets
            // Solution: Generate into a temporary buffer first
            scope_mgr.push_macro(macro_def, current_offset);
            let temp_res = Codegen::macro_to_bytecode(evm_version, macro_def, contract, scope_mgr, current_offset + 1, false, None, false)?;
            let macro_code_len = temp_res.bytes.iter().map(|seg| seg.bytes.len()).sum::<usize>();
            let stack_swaps_len = macro_def.returns;
            current_offset += macro_code_len + stack_swaps_len + 2; // JUMPDEST + MACRO_CODE_LEN + stack_swaps.len() + JUMP
            // Note: macro_to_bytecode already popped the macro (depth > 1)
        }

        // Second pass: Actually generate the function bytecode
        // Now all goto_ labels are registered, so nested calls will resolve
        for (macro_def, func_offset) in function_offsets {
            // Push the function to the scope
            scope_mgr.push_macro(macro_def, func_offset);

            // Add 1 to starting offset to account for the JUMPDEST opcode
            let mut res = Codegen::macro_to_bytecode(evm_version, macro_def, contract, scope_mgr, func_offset + 1, false, None, false)?;

            for j in res.unmatched_jumps.iter_mut() {
                let new_index = j.bytecode_index;
                j.bytecode_index = 0;
                let mut new_jumps = if let Some(jumps) = jump_table.get(&new_index) { jumps.clone() } else { vec![] };
                new_jumps.push(j.clone());
                jump_table.insert(new_index, new_jumps);
            }
            table_instances.extend(res.table_instances);
            label_indices.extend(res.label_indices);

            let macro_code_len = res.bytes.iter().map(|seg| seg.bytes.len()).sum::<usize>();

            // Get necessary swap ops to reorder stack
            // PC of the return jumpdest should be above the function's outputs on the stack
            let stack_swaps = (0..macro_def.returns).map(|i| format!("{:02x}", 0x90 + i)).collect::<Vec<_>>();

            // Insert JUMPDEST, stack swaps, and final JUMP back to the location of invocation.
            bytes.push_with_offset(func_offset, Bytes::Raw(Opcode::Jumpdest.to_string()));
            res.bytes.push_with_offset(func_offset + macro_code_len + 1, Bytes::Raw(format!("{}{}", stack_swaps.join(""), Opcode::Jump)));
            bytes.extend(res.bytes);

            // Note: The function is already popped by macro_to_bytecode at the end
            // because depth > 1 when generating functions. We should NOT pop again here.
        }

        *offset = current_offset;
        Ok(bytes)
    }

    /// Generate a codegen artifact
    ///
    /// # Arguments
    ///
    /// * `args` - A vector of Tokens representing constructor arguments
    /// * `main_bytecode` - The compiled MAIN Macro bytecode
    /// * `constructor_bytecode` - The compiled `CONSTRUCTOR` Macro bytecode
    #[allow(clippy::too_many_arguments)]
    pub fn churn(
        &mut self,
        file: Arc<FileSource>,
        mut args: Vec<alloy_dyn_abi::DynSolValue>,
        main_bytecode: &str,
        constructor_bytecode: &str,
        has_custom_bootstrap: bool,
        main_source_map: Option<Vec<SourceMapEntry>>,
        constructor_source_map: Option<Vec<SourceMapEntry>>,
    ) -> Result<Artifact, CodegenError> {
        let artifact: &mut Artifact = if let Some(art) = &mut self.artifact {
            art
        } else {
            self.artifact = Some(Artifact::default());
            self.artifact.as_mut().unwrap()
        };

        // Move `main_bytecode` to the heap so that it can be modified if need be.
        let mut main_bytecode = String::from(main_bytecode);

        let contract_length = main_bytecode.len() / 2;
        let constructor_length = constructor_bytecode.len() / 2;

        // Sort constructor arguments so that statically sized args are inserted last.
        args.sort_by(|a, b| {
            if a.is_dynamic() && !b.is_dynamic() {
                Ordering::Less
            } else if !a.is_dynamic() && b.is_dynamic() {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        });

        let mut arg_offset_acc = contract_length;
        let encoded: Vec<Vec<u8>> = args
            .into_iter()
            .enumerate()
            .map(|(i, tok)| {
                if tok.is_dynamic() {
                    let encoded = tok.abi_encode();

                    // Check for "__CODECOPY_DYN_ARG" calls for this specific argument. If any
                    // exist, fill the placeholders.
                    let tok_len = hex::encode(&encoded[62..64]);
                    let rep_regex = Regex::new(format!("xxxxxxxxxxxxxxxxxxxxxxxxxxxx{i:02x}\\d{{4}}").as_str()).unwrap();
                    rep_regex.find_iter(main_bytecode.clone().as_str()).for_each(|s| {
                        // TODO: Enforce that the arg type is a literal so that this unwrap is safe.
                        let len_ptr = usize::from_str_radix(&s.as_str()[30..34], 16).unwrap();
                        let contents_ptr = len_ptr + 0x20;

                        // Replace 17 reserved bytes.
                        main_bytecode.replace_range(
                            s.range(),
                            format!(
                                "{}{}{}{:04x}{}{}{}{}{:04x}{}{:04x}{}",
                                Opcode::Push2,    // PUSH2
                                &tok_len,         // len(bytes)
                                Opcode::Push2,    // PUSH2
                                len_ptr,          // <len_mem_ptr>
                                Opcode::Mstore,   // MSTORE
                                Opcode::Push2,    // PUSH2
                                &tok_len,         // len(bytes)
                                Opcode::Push2,    // PUSH2
                                arg_offset_acc,   // <contents_code_ptr>
                                Opcode::Push2,    // PUSH2
                                contents_ptr,     // <contents_mem_ptr>
                                Opcode::Codecopy  // CODECOPY
                            )
                            .as_str(),
                        );
                    });

                    // Increase argument offset accumulator.
                    arg_offset_acc += encoded.len() - 64;

                    // We don't need to store the pointer nor the length of dynamically sized
                    // elements in the code.
                    encoded[64..].into()
                } else {
                    tok.abi_encode()
                }
            })
            .collect();
        let hex_args: Vec<String> = encoded.iter().map(|tok| hex::encode(tok.as_slice())).collect();
        let constructor_args = hex_args.join("");

        // Sucks that we can't provide a span on this error. Need to refactor at some point.
        if main_bytecode.contains('x') {
            tracing::error!(target = "codegen", "Failed to fill `__CODECOPY_DYN_ARG` placeholders. Dynamic argument index is invalid.");
            return Err(CodegenError {
                kind: CodegenErrorKind::InvalidDynArgIndex,
                span: AstSpan(vec![Span { start: 0, end: 0, file: None }]).boxed(),
                token: None,
            });
        }

        // Constructor size optimizations
        let mut bootstrap_code_size = 9;
        let contract_size = if contract_length < 256 {
            // 60 = PUSH1
            format!("60{}", pad_n_bytes(format!("{contract_length:x}").as_str(), 1))
        } else {
            bootstrap_code_size += 1;
            // 61 = PUSH2
            format!("61{}", pad_n_bytes(format!("{contract_length:x}").as_str(), 2))
        };
        let contract_code_offset = if (bootstrap_code_size + constructor_length) < 256 {
            format!("60{}", pad_n_bytes(format!("{:x}", bootstrap_code_size + constructor_length).as_str(), 1))
        } else {
            bootstrap_code_size += 1;

            format!("61{}", pad_n_bytes(format!("{:x}", bootstrap_code_size + constructor_length).as_str(), 2))
        };

        // 80 = DUP1
        // 3d = RETURNDATASIZE, 39 = CODECOPY, 3d = RETURNDATASIZE, f3 = RETURN
        let bootstrap_code =
            if has_custom_bootstrap { String::default() } else { format!("{contract_size}80{contract_code_offset}3d393df3") };

        // Generate the final bytecode
        let constructor_code = format!("{constructor_bytecode}{bootstrap_code}");
        artifact.bytecode = format!("{constructor_code}{main_bytecode}{constructor_args}").to_lowercase();
        artifact.runtime = main_bytecode.to_lowercase();
        artifact.file = file;

        // Set source maps if provided
        artifact.runtime_map = main_source_map;
        artifact.constructor_map = constructor_source_map;

        Ok(artifact.clone())
    }

    /// Encode constructor arguments as alloy_dyn_abi::DynSolValue
    pub fn encode_constructor_args(args: Vec<String>) -> Vec<alloy_dyn_abi::DynSolValue> {
        let tokens: Vec<alloy_dyn_abi::DynSolValue> = args.iter().map(|tok| EToken::try_from(tok.clone()).unwrap().0).collect();
        tokens
    }

    /// Export
    ///
    /// Writes a Codegen Artifact out to the specified file.
    ///
    /// # Arguments
    ///
    /// * `out` - Output location to write the serialized json artifact to.
    pub fn export(output: String, art: &Artifact) -> Result<(), CodegenError> {
        let serialized_artifact = serde_json::to_string_pretty(art).unwrap();
        // Try to create the parent directory
        let file_path = Path::new(&output);
        if let Some(p) = file_path.parent()
            && let Err(e) = fs::create_dir_all(p)
        {
            return Err(CodegenError {
                kind: CodegenErrorKind::IOError(e.to_string()),
                span: AstSpan(vec![Span {
                    start: 0,
                    end: 0,
                    file: Some(Arc::new(FileSource { path: output, source: None, access: None, dependencies: vec![] })),
                }])
                .boxed(),
                token: None,
            });
        }
        if let Err(e) = fs::write(file_path, serialized_artifact) {
            return Err(CodegenError {
                kind: CodegenErrorKind::IOError(e.to_string()),
                span: AstSpan(vec![Span {
                    start: 0,
                    end: 0,
                    file: Some(Arc::new(FileSource { path: output, source: None, access: None, dependencies: vec![] })),
                }])
                .boxed(),
                token: None,
            });
        }
        Ok(())
    }

    /// Abi Generation
    ///
    /// Generates an ABI for the given Ast.
    /// Stores the generated ABI in the Codegen `artifact`.
    ///
    /// # Arguments
    ///
    /// * `ast` - The Contract Abstract Syntax Tree
    /// * `output` - An optional output path
    pub fn abi_gen(&mut self, ast: Contract, output: Option<String>) -> Result<Abi, CodegenError> {
        let abi: Abi = ast.into();

        // Set the abi on self
        let art: &Artifact = match &mut self.artifact {
            Some(artifact) => {
                artifact.abi = Some(abi.clone());
                artifact
            }
            None => {
                self.artifact = Some(Artifact { abi: Some(abi.clone()), ..Default::default() });
                self.artifact.as_ref().unwrap()
            }
        };

        // If an output's specified, write the artifact out
        if let Some(o) = output {
            // Error message is sent to tracing in `export` if an error occurs
            Codegen::export(o, art)?;
        }

        // Return the abi
        Ok(abi)
    }
}

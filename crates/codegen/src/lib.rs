#![doc = include_str!("../README.md")]
#![warn(missing_docs)]
#![warn(unused_extern_crates)]
#![forbid(unsafe_code)]

use crate::irgen::builtin_function::builtin_pad;
use alloy_primitives::{U256, hex};
use huff_neo_utils::ast::huff::*;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::bytes_util::str_to_bytes32;
use huff_neo_utils::file::file_source::FileSource;
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

/// Maximum allowed iterations for compile-time for loops to prevent infinite loops
const MAX_LOOP_ITERATIONS: usize = 10_000;

/// ### Codegen
///
/// Code Generation Manager responsible for generating bytecode from a
/// [Contract](../../huff_utils/src/ast.rs#Contract) Abstract Syntax Tree.
///
/// #### Usage
///
/// The canonical way to instantiate a Codegen instance is using the public associated
/// [new](Codegen::new) function.
///
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

    /// Generates main bytecode from a Contract AST
    pub fn generate_main_bytecode(
        evm_version: &EVMVersion,
        contract: &Contract,
        alternative_main: Option<String>,
    ) -> Result<String, CodegenError> {
        let (bytecode, _source_map) = Self::generate_main_bytecode_with_sourcemap(evm_version, contract, alternative_main)?;
        Ok(bytecode)
    }

    /// Generates main bytecode with source map from a Contract AST
    pub fn generate_main_bytecode_with_sourcemap(
        evm_version: &EVMVersion,
        contract: &Contract,
        alternative_main: Option<String>,
    ) -> Result<(String, Vec<SourceMapEntry>), CodegenError> {
        // Expand for loops before any other processing
        let contract = Self::expand_for_loops(contract)?;
        // Expand if statements before any other processing
        let contract = Self::expand_if_statements(&contract)?;
        let contract = Self::update_table_size(evm_version, &contract)?;

        // If an alternative main is provided, then use it as the compilation target
        let main_macro = alternative_main.unwrap_or_else(|| String::from("MAIN"));

        // Find the main macro
        let m_macro = Codegen::get_macro_by_name(&main_macro, &contract)?;

        // For each MacroInvocation Statement, recurse into bytecode
        let bytecode_res: BytecodeRes =
            Codegen::macro_to_bytecode(evm_version, m_macro, &contract, &mut vec![m_macro], 0, &mut Vec::default(), false, None)?;

        tracing::debug!(target: "codegen", "Generated main bytecode. Appending table bytecode...");

        // Generate the fully baked bytecode
        Codegen::gen_table_bytecode(&contract, bytecode_res)
    }

    /// Generates constructor bytecode from a Contract AST
    pub fn generate_constructor_bytecode(
        evm_version: &EVMVersion,
        contract: &Contract,
        alternative_constructor: Option<String>,
    ) -> Result<(String, bool), CodegenError> {
        let ((bytecode, _source_map), has_custom) =
            Self::generate_constructor_bytecode_with_sourcemap(evm_version, contract, alternative_constructor)?;
        Ok((bytecode, has_custom))
    }

    /// Generates constructor bytecode with source map from a Contract AST
    pub fn generate_constructor_bytecode_with_sourcemap(
        evm_version: &EVMVersion,
        contract: &Contract,
        alternative_constructor: Option<String>,
    ) -> Result<((String, Vec<SourceMapEntry>), bool), CodegenError> {
        // Expand for loops before any other processing
        let contract = Self::expand_for_loops(contract)?;
        // Expand if statements before any other processing
        let contract = Self::expand_if_statements(&contract)?;
        let contract = Self::update_table_size(evm_version, &contract)?;

        // If an alternative constructor macro is provided, then use it as the compilation target
        let constructor_macro = alternative_constructor.unwrap_or_else(|| String::from("CONSTRUCTOR"));

        // Find the constructor macro
        let c_macro = Codegen::get_macro_by_name(&constructor_macro, &contract)?;

        // For each MacroInvocation Statement, recurse into bytecode
        let bytecode_res: BytecodeRes =
            Codegen::macro_to_bytecode(evm_version, c_macro, &contract, &mut vec![c_macro], 0, &mut Vec::default(), false, None)?;

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

    /// Expand all for loops in the contract at compile-time
    /// This must be called before bytecode generation
    pub fn expand_for_loops(contract: &Contract) -> Result<Contract, CodegenError> {
        let mut contract = contract.clone();

        // Expand loops in all macros
        let mut expanded_macros = indexmap::IndexMap::new();
        for (name, macro_def) in contract.macros.iter() {
            let expanded_statements = Self::expand_statements_for_loops(&macro_def.statements, &contract)?;
            let mut new_macro = macro_def.clone();
            new_macro.statements = expanded_statements;
            expanded_macros.insert(name.clone(), new_macro);
        }
        contract.macros = expanded_macros;

        // Expand loops in tables
        let mut expanded_tables = Vec::new();
        for table in contract.tables.iter() {
            let expanded_statements = Self::expand_statements_for_loops(&table.statements, &contract)?;
            let mut new_table = table.clone();
            new_table.statements = expanded_statements;
            expanded_tables.push(new_table);
        }
        contract.tables = expanded_tables;

        Ok(contract)
    }

    /// Recursively expand for loops in a list of statements
    fn expand_statements_for_loops(statements: &[Statement], contract: &Contract) -> Result<Vec<Statement>, CodegenError> {
        let mut expanded = Vec::new();

        for statement in statements {
            match &statement.ty {
                StatementType::ForLoop { variable, start, end, step, body } => {
                    // Evaluate loop bounds at compile-time
                    let start_val = contract.evaluate_constant_expression(start)?;
                    let end_val = contract.evaluate_constant_expression(end)?;
                    let step_val = if let Some(step_expr) = step {
                        contract.evaluate_constant_expression(step_expr)?
                    } else {
                        // Default step is 1
                        str_to_bytes32("1")
                    };

                    // Convert to U256 for iteration (supports full u256 range)
                    let start = U256::from_be_bytes(start_val);
                    let end = U256::from_be_bytes(end_val);
                    let step = U256::from_be_bytes(step_val);

                    if step.is_zero() {
                        return Err(CodegenError {
                            kind: CodegenErrorKind::InvalidLoopStep,
                            span: statement.span.clone_box(),
                            token: None,
                        });
                    }

                    // Generate iterations with safety limit
                    let mut current = start;
                    let mut iteration_count = 0;

                    while current < end {
                        if iteration_count >= MAX_LOOP_ITERATIONS {
                            return Err(CodegenError {
                                kind: CodegenErrorKind::LoopIterationLimitExceeded(MAX_LOOP_ITERATIONS),
                                span: statement.span.clone_box(),
                                token: None,
                            });
                        }

                        // Clone body and substitute loop variable
                        let current_bytes = current.to_be_bytes::<32>();
                        let iteration_body = Self::substitute_loop_variable(body, variable, &current_bytes)?;
                        // Recursively expand in case of nested loops
                        let expanded_body = Self::expand_statements_for_loops(&iteration_body, contract)?;
                        expanded.extend(expanded_body);

                        current = current.saturating_add(step);
                        iteration_count += 1;
                    }
                }
                StatementType::Label(label) => {
                    // Recursively expand loops inside labels
                    let expanded_inner = Self::expand_statements_for_loops(&label.inner, contract)?;
                    let mut new_label = label.clone();
                    new_label.inner = expanded_inner;
                    expanded.push(Statement { ty: StatementType::Label(new_label), span: statement.span.clone() });
                }
                _ => {
                    // Keep other statements as-is
                    expanded.push(statement.clone());
                }
            }
        }

        Ok(expanded)
    }

    /// Substitute loop variable in statement body
    fn substitute_loop_variable(statements: &[Statement], var_name: &str, value: &[u8; 32]) -> Result<Vec<Statement>, CodegenError> {
        let mut substituted = Vec::new();
        let var_literal = *value;

        for statement in statements {
            let new_statement = match &statement.ty {
                StatementType::Constant(name) => {
                    // Check if this is a loop variable placeholder
                    if name == &format!("__LOOP_VAR_{}", var_name) {
                        // Replace with literal
                        Statement { ty: StatementType::Literal(var_literal), span: statement.span.clone() }
                    } else {
                        statement.clone()
                    }
                }
                StatementType::Label(label) => {
                    // Recursively substitute in label body
                    let substituted_inner = Self::substitute_loop_variable(&label.inner, var_name, value)?;
                    let mut new_label = label.clone();
                    new_label.inner = substituted_inner;
                    // Rename label to prevent collisions in different iterations
                    let iter_value = U256::from_be_bytes(*value);
                    new_label.name = format!("{}_{}", label.name, iter_value);
                    Statement { ty: StatementType::Label(new_label), span: statement.span.clone() }
                }
                StatementType::LabelCall(label_name) => {
                    // Update label references to match renamed labels. Wwe rename based on the current iteration.
                    let iter_value = U256::from_be_bytes(*value);
                    Statement { ty: StatementType::LabelCall(format!("{}_{}", label_name, iter_value)), span: statement.span.clone() }
                }
                StatementType::ForLoop { variable: loop_var, start, end, step, body } => {
                    // Recursively substitute in nested for loop body
                    let substituted_body = Self::substitute_loop_variable(body, var_name, value)?;
                    Statement {
                        ty: StatementType::ForLoop {
                            variable: loop_var.clone(),
                            start: start.clone(),
                            end: end.clone(),
                            step: step.clone(),
                            body: substituted_body,
                        },
                        span: statement.span.clone(),
                    }
                }
                _ => statement.clone(),
            };
            substituted.push(new_statement);
        }

        Ok(substituted)
    }

    /// Expand all if statements in the contract at compile-time
    /// This must be called before bytecode generation
    pub fn expand_if_statements(contract: &Contract) -> Result<Contract, CodegenError> {
        let mut contract = contract.clone();

        // Expand if statements in all macros
        let mut expanded_macros = indexmap::IndexMap::new();
        for (name, macro_def) in contract.macros.iter() {
            let expanded_statements = Self::expand_statements_if_statements(&macro_def.statements, &contract)?;
            let mut new_macro = macro_def.clone();
            new_macro.statements = expanded_statements;
            expanded_macros.insert(name.clone(), new_macro);
        }
        contract.macros = expanded_macros;

        // Expand if statements in tables
        let mut expanded_tables = Vec::new();
        for table in contract.tables.iter() {
            let expanded_statements = Self::expand_statements_if_statements(&table.statements, &contract)?;
            let mut new_table = table.clone();
            new_table.statements = expanded_statements;
            expanded_tables.push(new_table);
        }
        contract.tables = expanded_tables;

        Ok(contract)
    }

    /// Recursively expand if statements in a list of statements
    fn expand_statements_if_statements(statements: &[Statement], contract: &Contract) -> Result<Vec<Statement>, CodegenError> {
        let mut expanded = Vec::new();

        for statement in statements {
            match &statement.ty {
                StatementType::IfStatement { condition, then_branch, else_if_branches, else_branch } => {
                    // Evaluate condition at compile-time
                    let condition_val = contract.evaluate_constant_expression(condition)?;
                    let condition_true = !U256::from_be_bytes(condition_val).is_zero();

                    if condition_true {
                        // Include then branch
                        let expanded_then = Self::expand_statements_if_statements(then_branch, contract)?;
                        expanded.extend(expanded_then);
                    } else {
                        // Check else if branches
                        let mut matched = false;
                        for (else_if_condition, else_if_body) in else_if_branches {
                            let else_if_val = contract.evaluate_constant_expression(else_if_condition)?;
                            let else_if_true = !U256::from_be_bytes(else_if_val).is_zero();

                            if else_if_true {
                                // Include this else if branch
                                let expanded_else_if = Self::expand_statements_if_statements(else_if_body, contract)?;
                                expanded.extend(expanded_else_if);
                                matched = true;
                                break;
                            }
                        }

                        // If no else if matched, include else branch if present
                        if !matched && let Some(else_body) = else_branch {
                            let expanded_else = Self::expand_statements_if_statements(else_body, contract)?;
                            expanded.extend(expanded_else);
                        }
                    }
                }
                StatementType::Label(label) => {
                    // Recursively expand if statements inside labels
                    let expanded_inner = Self::expand_statements_if_statements(&label.inner, contract)?;
                    let mut new_label = label.clone();
                    new_label.inner = expanded_inner;
                    expanded.push(Statement { ty: StatementType::Label(new_label), span: statement.span.clone() });
                }
                StatementType::ForLoop { variable, start, end, step, body } => {
                    // Recursively expand if statements inside for loop bodies
                    let expanded_body = Self::expand_statements_if_statements(body, contract)?;
                    expanded.push(Statement {
                        ty: StatementType::ForLoop {
                            variable: variable.clone(),
                            start: start.clone(),
                            end: end.clone(),
                            step: step.clone(),
                            body: expanded_body,
                        },
                        span: statement.span.clone(),
                    });
                }
                _ => {
                    // Keep other statements as-is
                    expanded.push(statement.clone());
                }
            }
        }

        Ok(expanded)
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
            if let Some(o) = table_offsets.get(&jump.label) {
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

    /// Recurses a MacroDefinition to generate Bytecode
    ///
    /// ## Overview
    ///
    /// `macro_to_bytecode` first transforms the macro definition into "IR" Bytecode - a vec of
    /// intermediate bytes. It then iterates over each byte, converting the
    /// [IRByte](struct.IRByte.html) into a `Bytes`. Once done iterating over the macro
    /// definition IRBytes, we use the JumpTable to match any unmatched jumps. If jumps are not
    /// matched, they are appended to a vec of unmatched jumps.
    ///
    /// On success, a [BytecodeRes](struct.BytecodeRes.html) is returned,
    /// containing the generated bytes, label indices, unmatched jumps, and table indices.
    ///
    /// ## Arguments
    ///
    /// * `macro_def` - Macro definition to convert to bytecode
    /// * `contract` - Reference to the `Contract` AST generated by the parser
    /// * `scope` - Current scope of the recursion. Contains all macro definitions recursed so far.
    /// * `offset` - Current bytecode offset
    /// * `mis` - Vector of tuples containing parent macro invocations as well as their offsets.
    #[allow(clippy::too_many_arguments)]
    pub fn macro_to_bytecode<'a>(
        evm_version: &EVMVersion,
        macro_def: &'a MacroDefinition,
        contract: &'a Contract,
        scope: &mut Vec<&'a MacroDefinition>,
        mut offset: usize,
        mis: &mut Vec<(usize, MacroInvocation)>,
        recursing_constructor: bool,
        circular_codesize_invocations: Option<&mut CircularCodeSizeIndices>,
    ) -> Result<BytecodeRes, CodegenError> {
        // Get intermediate bytecode representation of the macro definition
        let mut bytes = BytecodeSegments::new();
        let mut spans: Vec<Option<(usize, usize)>> = Vec::default();
        let ir_bytes = macro_def.to_irbytecode(evm_version)?.0;

        // Define outer loop variables
        let mut jump_table = JumpTable::new();
        let mut label_indices = LabelIndices::new();
        let mut table_instances = Jumps::new();
        let mut utilized_tables: Vec<TableDefinition> = Vec::new();
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
                        scope,
                        &mut offset,
                        mis,
                        &mut jump_table,
                        &mut label_indices,
                        &mut table_instances,
                        &mut utilized_tables,
                        circular_codesize_invocations,
                        starting_offset,
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
                        scope,
                        &mut offset,
                        mis,
                        &mut jump_table,
                        &mut table_instances,
                        &mut utilized_tables,
                        ir_byte.span,
                    )?;
                    // Add span for each byte segment added by bubble_arg_call
                    let bytes_added = bytes.len() - bytes_before;
                    for _ in 0..bytes_added {
                        spans.push(span_info);
                    }
                }
            }
        }

        // We're done, let's pop off the macro invocation
        if mis.pop().is_none() {
            tracing::warn!(target: "codegen", "ATTEMPTED MACRO INVOCATION POP FAILED AT SCOPE: {}", scope.len());
        }

        // Add functions (outlined macros) to the end of the bytecode if the scope length == 1
        // (i.e., we're at the top level of recursion)
        if scope.len() == 1 {
            let bytes_before = bytes.len();
            bytes = Codegen::append_functions(
                evm_version,
                contract,
                scope,
                &mut offset,
                mis,
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
            scope.pop();
        }

        // Fill JUMPDEST placeholders
        let (new_bytes, unmatched_jumps) = Codegen::fill_unmatched(bytes.clone(), &jump_table, &label_indices)?;

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
        Ok(BytecodeRes { bytes, spans, label_indices, unmatched_jumps, table_instances, utilized_tables })
    }

    /// Helper associated function to fill unmatched jump dests.
    ///
    /// ## Overview
    ///
    /// Iterates over the vec of generated bytes. At each index, check if a jump is tracked.
    /// If one is, find the index of label and inplace the formatted location.
    /// If there is no label matching the jump, we append the jump to a list of unmatched jumps,
    /// updating the jump's bytecode index.
    ///
    /// On success, returns a tuple of generated bytes and unmatched jumps.
    /// On failure, returns a CodegenError.
    #[allow(clippy::type_complexity)]
    pub fn fill_unmatched(
        mut bytes: BytecodeSegments,
        jump_table: &JumpTable,
        label_indices: &LabelIndices,
    ) -> Result<(BytecodeSegments, Vec<Jump>), CodegenError> {
        // Resolve all jumps using the new typed approach
        let unmatched_jumps = bytes.resolve_jumps(jump_table, label_indices).map_err(|label_error| {
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
                "UNMATCHED JUMP LABEL \"{}\" AT BYTECODE INDEX {} (scope_depth={}, scope_path={:?})",
                jump.label, jump.bytecode_index, jump.scope_depth, jump.scope_path
            );
        }

        Ok((bytes, unmatched_jumps))
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
        scope: &mut Vec<&'a MacroDefinition>,
        offset: &mut usize,
        mis: &mut Vec<(usize, MacroInvocation)>,
        jump_table: &mut JumpTable,
        label_indices: &mut LabelIndices,
        table_instances: &mut Jumps,
        mut bytes: BytecodeSegments,
    ) -> Result<BytecodeSegments, CodegenError> {
        for macro_def in contract.macros.values().filter(|m| m.outlined) {
            // Push the function to the scope
            scope.push(macro_def);

            // Add 1 to starting offset to account for the JUMPDEST opcode
            let mut res = Codegen::macro_to_bytecode(evm_version, macro_def, contract, scope, *offset + 1, mis, false, None)?;

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
            bytes.push_with_offset(*offset, Bytes::Raw(Opcode::Jumpdest.to_string()));
            res.bytes.push_with_offset(*offset + macro_code_len + 1, Bytes::Raw(format!("{}{}", stack_swaps.join(""), Opcode::Jump)));
            bytes.extend(res.bytes);
            // Add the jumpdest to the beginning of the outlined macro.
            // Outlined macros are at the top-level scope
            let scope_path: Vec<String> = vec![];
            let scope_depth = 0;
            if let Err(err_msg) = label_indices.insert(format!("goto_{}", macro_def.name.clone()), *offset, scope_depth, scope_path) {
                // This shouldn't happen for auto-generated goto_ labels - indicates a compiler bug
                tracing::error!(target: "codegen", "INTERNAL ERROR: Duplicate goto_ label for function '{}': {}", macro_def.name, err_msg);
                return Err(CodegenError {
                    kind: CodegenErrorKind::DuplicateLabelInScope(format!("goto_{}", macro_def.name)),
                    span: macro_def.span.clone_box(),
                    token: None,
                });
            }
            *offset += macro_code_len + stack_swaps.len() + 2; // JUMPDEST + MACRO_CODE_LEN + stack_swaps.len() + JUMP
        }
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
